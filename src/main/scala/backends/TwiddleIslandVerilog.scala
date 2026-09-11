package backends

import ir.rtl.*
import scala.collection.mutable

/** Move only constant-table coefficient cones, never data arithmetic or a clock edge.
  * Address equality includes its existing schedule; unlike floating-point folding,
  * every intermediate operation is reduced to its original RTL width.
  */
object TwiddleIslandVerilog:
  case class Table(address: Option[Component], cycles: Int, width: Int, values: Vector[BigInt])
  case class Emission(definitions: String, instances: String,
      operands: Map[(Times, Component), String])

  /** Store each Boolean column only once locally, including complements.
    * A four-entry table has at most seven nonconstant complementary pairs.
    * This shares neither registers nor wide coefficients across islands.
    */
  case class Encoding(columns: Vector[Int], bits: Vector[(Int, Boolean)], encoded: Vector[BigInt])
  def encode(table: Table): Encoding =
    val full = (1 << table.values.size) - 1
    val columns = mutable.ArrayBuffer.empty[Int]
    val bits = (0 until table.width).map { bit =>
      val column = table.values.zipWithIndex.foldLeft(0) { case (acc, (value, row)) =>
        acc | (if value.testBit(bit) then 1 << row else 0)
      }
      val key = math.min(column, column ^ full)
      val inverted = column != key
      if key == 0 then (-1, inverted)
      else
        if !columns.contains(key) then columns += key
        (columns.indexOf(key), inverted)
    }.toVector
    val encoded = table.values.indices.map(row => columns.zipWithIndex.foldLeft(BigInt(0)) {
      case (acc, (column, index)) => if (column & (1 << row)) != 0 then acc.setBit(index) else acc
    }).toVector
    Encoding(columns.toVector, bits, encoded)

  def analyze(components: Seq[Component]): Map[Component, Table] =
    val memo = mutable.Map.empty[Component, Option[Table]]
    val active = mutable.Set.empty[Component]
    def mask(width: Int) = (BigInt(1) << width) - 1
    def combine(width: Int, terms: Seq[Table])(f: Seq[BigInt] => BigInt): Option[Table] =
      val dynamic = terms.filter(_.address.nonEmpty)
      val shape = dynamic.headOption.getOrElse(Table(None, 0, width, Vector(BigInt(0))))
      if !dynamic.forall(t => t.address == shape.address && t.cycles == shape.cycles) then None
      else Some(Table(shape.address, shape.cycles, width,
        shape.values.indices.map(i => f(terms.map(t => t.values(if t.address.isEmpty then 0 else i))) & mask(width)).toVector))
    def all(cs: Seq[Component]): Option[Seq[Table]] =
      val ts = cs.map(visit)
      if ts.forall(_.nonEmpty) then Some(ts.flatten) else None
    def visit(c: Component): Option[Table] =
      if active(c) then None
      else memo.getOrElseUpdate(c, {
        active += c
        val result = c match
          case Const(w, bits) => Some(Table(None, 0, w, Vector(bits & mask(w))))
          case Wire(input) => visit(input)
          case Register(input, n) => visit(input).map(t =>
            if t.address.isEmpty then t else t.copy(cycles = t.cycles + n))
          case Mux(address, inputs) if Set(1, 2)(address.size) &&
              inputs.size == (1 << address.size) && inputs.forall(_.isInstanceOf[Const]) =>
            Some(Table(Some(address), 0, c.size, inputs.map(_.asInstanceOf[Const].value & mask(c.size)).toVector))
          case Plus(terms) => all(terms).flatMap(combine(c.size, _)(_.sum))
          case Minus(a, b) => all(Seq(a, b)).flatMap(combine(c.size, _)(v => v(0) - v(1)))
          case Tap(input, range) if range.step == 1 => visit(input).map(t =>
            t.copy(width = c.size, values = t.values.map(v => (v >> range.start) & mask(c.size))))
          case Concat(inputs) => all(inputs).flatMap(ts => combine(c.size, ts)(values =>
            values.zip(inputs).foldLeft(BigInt(0)) { case (acc, (v, in)) => (acc << in.size) | v }))
          case _ => None
        active -= c
        result
      })
    components.foreach(visit)
    memo.collect { case (c, Some(t)) => c -> t }.toMap

  def emit(components: Seq[Component], name: Component => String, maxConsumers: Int): Emission =
    require(Set(0, 4)(maxConsumers), "twiddle island consumers must be 0 or 4")
    if maxConsumers == 0 then return Emission("", "", Map.empty)
    val tables = analyze(components)
    val indexes = components.zipWithIndex.toMap
    val eligible = components.collect { case m: Times => m }.flatMap(m =>
      Seq(m.lhs, m.rhs).distinct.flatMap(c => tables.get(c).filter(t =>
        t.address.nonEmpty && t.cycles > 0 && t.width > 2 && t.values.distinct.size > 1).map(t => (t, m, c))))
    val definitions = new StringBuilder
    val instances = new StringBuilder
    val operands = mutable.Map.empty[(Times, Component), String]
    val regions = mutable.Map.empty[Component, Vector[(String, Int)]]
    var regionOrdinal = 0
    def previousEdge(c: Component): Option[String] = c match
      case Wire(input) => previousEdge(input)
      case Register(input, 1) => Some(name(input))
      case Tap(input, range) if range.step == 1 => previousEdge(input).map(s => s"($s >> ${range.start})")
      case _ => None
    def localAddress(address: Component): String = previousEdge(address) match
      case None => name(address)
      case Some(input) =>
        var copies = regions.getOrElse(address, Vector.empty)
        if copies.isEmpty || copies.last._2 == 32 then
          val id = s"twiddle_region_$regionOrdinal"
          regionOrdinal += 1
          copies :+= ((id, 0))
          instances ++= s"// twiddle_region name=$id address=${name(address)} width=${address.size} predecessor=$input\n"
          instances ++= s"  wire [${address.size - 1}:0] ${id}_value;\n  SGenTwiddleAddressRegion #(.WIDTH(${address.size})) $id(.clk(clk), .d($input), .q(${id}_value));\n"
        val (id, count) = copies.last
        regions(address) = copies.init :+ ((id, count + 1))
        s"${id}_value"
    var ordinal = 0
    eligible.groupBy(_._1).toSeq.sortBy { case (_, entries) => entries.map(e => indexes(e._2)).min }
      .foreach { case (table, entries) =>
        entries.sortBy(e => (indexes(e._2), indexes(e._3))).grouped(maxConsumers).foreach { group =>
          val id = s"twiddle_island_$ordinal"
          val module = s"SGenForwardTwiddleIsland_$ordinal"
          val addressWidth = table.address.get.size
          val encoding = encode(table)
          val stored = encoding.columns.size
          val delay = table.cycles - 1
          val phaseInput = localAddress(table.address.get)
          val selector = if delay == 0 then "phase" else s"phase_${delay - 1}"
          definitions ++= s"""(* KEEP_HIERARCHY="yes", DONT_TOUCH="yes" *) module $module(
  input clk, input [${addressWidth - 1}:0] phase,
  output wire [${table.width - 1}:0] value);
  (* KEEP="yes", DONT_TOUCH="yes", SHREG_EXTRACT="no" *) reg [${stored - 1}:0] coefficient;
  reg [${stored - 1}:0] decoded;
"""
          for i <- 0 until delay do
            definitions ++= s"  (* KEEP=\"yes\", DONT_TOUCH=\"yes\", SHREG_EXTRACT=\"no\" *) reg [${addressWidth - 1}:0] phase_$i;\n"
          definitions ++= s"  always @* begin\n    case ($selector)\n"
          encoding.encoded.zipWithIndex.foreach { (value, index) =>
            definitions ++= s"      ${addressWidth}'d$index: decoded = ${stored}'d$value;\n"
          }
          definitions ++= s"      default: decoded = ${stored}'bx;\n    endcase\n  end\n  always @(posedge clk) begin\n"
          for i <- 0 until delay do
            definitions ++= s"    phase_$i <= ${if i == 0 then "phase" else s"phase_${i - 1}"};\n"
          definitions ++= "    coefficient <= decoded;\n  end\n"
          encoding.bits.zipWithIndex.foreach { case ((index, inverted), bit) =>
            val expression = if index < 0 then (if inverted then "1'b1" else "1'b0")
              else s"${if inverted then "~" else ""}coefficient[$index]"
            definitions ++= s"  assign value[$bit] = $expression;\n"
          }
          definitions ++= "endmodule\n"
          instances ++= s"// twiddle_island name=$id address=${name(table.address.get)} cycles=${table.cycles} width=${table.width} stored=$stored consumers=${group.map(e => name(e._2)).mkString(",")} sources=${group.map(e => name(e._3)).distinct.mkString(",")}\n"
          instances ++= s"  wire [${table.width - 1}:0] ${id}_value;\n  $module $id(.clk(clk), .phase($phaseInput), .value(${id}_value));\n"
          group.foreach { (_, m, c) => operands((m, c)) = s"${id}_value" }
          ordinal += 1
        }
      }
    components.collect { case m: Times => m }.foreach { m =>
      if !Seq(m.lhs, m.rhs).exists(c => operands.contains((m, c))) then
        instances ++= s"// twiddle_unhandled multiplier=${name(m)} reason=no_aligned_shallow_registered_table\n"
    }
    if regions.nonEmpty then definitions ++= """(* KEEP_HIERARCHY="yes", DONT_TOUCH="yes" *)
module SGenTwiddleAddressRegion #(parameter integer WIDTH=2)(
  input clk, input [WIDTH-1:0] d, output wire [WIDTH-1:0] q);
  (* KEEP="yes", DONT_TOUCH="yes", SHREG_EXTRACT="no" *) reg [WIDTH-1:0] value;
  always @(posedge clk) value <= d;
  assign q = value;
endmodule
"""
    Emission(definitions.toString, instances.toString, operands.toMap)
