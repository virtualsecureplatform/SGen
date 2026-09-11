package backends

import ir.rtl.*

/** Frame descriptors follow exactly the same delay as the corresponding token.
  * The counter output remains an ordinary Register for selector replication.
  */
object FrameControlVerilog:
  def emit(counters: Seq[FrameCounterValue], name: Component => String): (String, Map[FrameCounterValue, String]) =
    if counters.isEmpty then return ("", Map.empty)
    def delay(c: Component): Int = c match
      case Input(1, "next") => 0
      case Wire(input) => delay(input)
      case Register(input, cycles) => delay(input) + cycles
      case _ => throw new IllegalArgumentException("frame descriptor requires an unmodified delayed next token")
    def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
    val period = counters.map(_.limit).distinct.foldLeft(1)((a,b) => a / gcd(a,b) * b)
    require(period <= 256, "unsupported frame descriptor period")
    val width = BigInt(period - 1).bitLength
    val maxDelay = counters.flatMap(c => Seq(delay(c.first), delay(c.trigger))).max
    def descriptor(d: Int) = if d == 0 then "frame_ordinal" else s"frame_descriptor[${d-1}]"
    def lookup(c: FrameCounterValue, offset: Int, d: Int): String =
      s"frame_mod_${c.limit}_$offset(${descriptor(d)})"
    val functions = counters.flatMap(c => Seq((c.limit, c.resetValue), (c.limit, (c.resetValue+1)%c.limit))).distinct.sorted.map { (limit, offset) =>
      val bits = BigInt(limit-1).bitLength
      val cases = (0 until period).map(i => s"      $width'd$i: frame_mod_${limit}_$offset = $bits'd${(i+offset)%limit};").mkString("\n")
      s"""  function [$bits-1:0] frame_mod_${limit}_$offset(input [$width-1:0] ordinal);
         |    case (ordinal)
         |$cases
         |      default: frame_mod_${limit}_$offset = $bits'd0;
         |    endcase
         |  endfunction
         |""".stripMargin
    }.mkString
    val pipe = if maxDelay == 0 then "" else
      s"""  reg [$width-1:0] frame_descriptor [0:${maxDelay-1}];
         |  integer frame_delay;
         |  always @(posedge clk) begin
         |    frame_descriptor[0] <= frame_ordinal;
         |    for (frame_delay = 1; frame_delay < $maxDelay; frame_delay = frame_delay + 1)
         |      frame_descriptor[frame_delay] <= frame_descriptor[frame_delay-1];
         |  end
         |""".stripMargin
    val manifest = counters.map(c => s"// FRAME_COUNTER name=${name(c)} period=${c.limit} reset_value=${c.resetValue} first_delay=${delay(c.first)} trigger_delay=${delay(c.trigger)} late=${c.late}").sorted.mkString("\n")
    val declarations = s"""// FRAME_CONTROL mode=token period=$period descriptor_bits=$width max_delay=$maxDelay
       |$manifest
       |  reg [$width-1:0] frame_ordinal;
       |  always @(posedge clk) begin
       |    if (reset) frame_ordinal <= 0;
       |    else if (next) frame_ordinal <= frame_ordinal == $width'd${period-1} ? $width'd0 : frame_ordinal + 1'b1;
       |  end
       |$pipe$functions
       |""".stripMargin
    val expressions = counters.map { c =>
      val onTrigger = lookup(c, (c.resetValue+1)%c.limit, delay(c.trigger))
      val onFirst = lookup(c, c.resetValue, delay(c.first))
      val normal = s"${name(c.trigger)} ? $onTrigger : ${name(c.previous)}"
      c -> (if c.late then s"${name(c.first)} ? $onFirst : ($normal)" else normal)
    }.toMap
    (declarations, expressions)
