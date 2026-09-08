package transforms.perm

import ir.rtl.{Component, BankedPermutationTile, RAMControl, StreamingModule, Tap}
import ir.rtl.hardwaretype.HW
import maths.fields.F2
import maths.linalg.Matrix
import transforms.Transform

/** One edge is a word, connecting its input cycle to its output cycle. */
case class BankedWord(input: Int, output: Int):
  def writeBeat: Int = input / 128
  def readBeat: Int = output / 128

case class BankedTile(role: String, ordinal: Int, inputs: Vector[Int], outputs: Vector[Int], banks: Vector[Vector[BankedWord]]):
  val moduleName = s"SGenBankedPermutation_${role}_$ordinal"
  require(inputs.size == 4 && outputs.size == 4 && banks.size == 4)
  require(banks.forall(b => b.size == 4 && b.map(_.writeBeat).sorted == Vector(0,1,2,3) && b.map(_.readBeat).sorted == Vector(0,1,2,3)))
  require(banks.flatten.map(_.input).distinct.size == 16 && banks.flatten.map(_.output).distinct.size == 16)

object BankedPermutation:
  /** Connected input/output lane components followed by regular bipartite edge colouring. */
  def tiles(p: Matrix[F2], role: String): Vector[BankedTile] =
    require(p.m == 9 && p.n == 9 && p.isInvertible)
    require(role.matches("[A-Za-z][A-Za-z0-9_]*"))
    val words = Vector.tabulate(512)(i => BankedWord(i, LinearPerm.permute(p, i)))
    val remaining = scala.collection.mutable.Set.from(0 until 128)
    val result = Vector.newBuilder[BankedTile]
    var ordinal = 0
    while remaining.nonEmpty do
      var ins = Set(remaining.min)
      var outs = Set.empty[Int]
      var changed = true
      while changed do
        val connected = words.filter(w => ins(w.input % 128) || outs(w.output % 128))
        val nextIns = ins ++ connected.map(_.input % 128)
        val nextOuts = outs ++ connected.map(_.output % 128)
        changed = nextIns != ins || nextOuts != outs
        ins = nextIns
        outs = nextOuts
      require(ins.size == 4 && outs.size == 4, s"$role: expected four-lane component, got ${ins.size}/${outs.size}")
      remaining --= ins
      var unused = words.filter(w => ins(w.input % 128))
      val banks = Vector.tabulate(4) { _ =>
        def matching(beat: Int, usedRead: Set[Int]): Option[Vector[BankedWord]] =
          if beat == 4 then Some(Vector.empty)
          else unused.iterator.filter(w => w.writeBeat == beat && !usedRead(w.readBeat))
            .map(w => matching(beat + 1, usedRead + w.readBeat).map(w +: _)).collectFirst { case Some(m) => m }
        val bank = matching(0, Set.empty).getOrElse(throw new IllegalArgumentException(s"$role: no perfect matching"))
        unused = unused.filterNot(bank.contains)
        bank
      }
      require(unused.isEmpty)
      result += BankedTile(role, ordinal, ins.toVector.sorted, outs.toVector.sorted, banks)
      ordinal += 1
    val tiles = result.result()
    require(tiles.size == 32)
    tiles

/** Opaque SPL boundary: generic permutation fusion must not erase the tile. */
case class BankedPermutation[T](p: Matrix[F2], role: String, localAdmission: Boolean = false, commutator: Boolean = false) extends Transform[T](9):
  val plan: Vector[BankedTile] = BankedPermutation.tiles(p, role)
  if commutator then plan.foreach { tile =>
    require(tile.banks.flatten.forall(w =>
      w.readBeat == tile.inputs.indexOf(w.input % 128) &&
        tile.outputs.indexOf(w.output % 128) == w.writeBeat),
      s"$role/${tile.ordinal}: commutator requires a pure four-by-four transpose")
  }
  override def eval(inputs: Seq[T], set: Int): Seq[T] = LinearPerm.permute(p, inputs)
  override def stream(k: Int, control: RAMControl)(using HW[T]): StreamingModule[T] =
    require(k == 7, "banked permutations require 512 points / 128 lanes")
    val transform = this
    new StreamingModule[T](2, 7):
      override val latency = if commutator then 4 else 6
      override val spl = transform
      override def implement(rst: Component, token: Int => Component, inputs: Seq[Component]): Seq[Component] =
        val mapped = plan.flatMap { tile =>
          // Admission is registered inside the local-control tile. Request its
          // token early so the register does not add a data/transaction cycle.
          val packed = BankedPermutationTile(tile.inputs.map(inputs(_)), token(if localAdmission || commutator then -1 else 0), rst, tile, hw.size, localAdmission || commutator, commutator)
          tile.outputs.zipWithIndex.map((lane, local) => lane -> Tap(packed, local * hw.size until (local + 1) * hw.size))
        }.toMap
        Vector.tabulate(K)(mapped)
