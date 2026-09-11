package backends

import org.scalatest.funsuite.AnyFunSuite
import transforms.perm.BankedPermutation

class CommutatorScheduleTest extends AnyFunSuite:
  private case class DelayUnit(half: Int, upper: Vector[Vector[Int]], lower: Vector[Vector[Int]]):
    def output(data: Vector[Int], select: Boolean): Vector[Int] =
      lower.map(_.last) ++ upper.indices.map(i => if select then data(i) else upper(i).last)
    def shift(data: Vector[Int], select: Boolean): DelayUnit = copy(
      upper = upper.indices.map(i => data(i+half) +: upper(i).dropRight(1)).toVector,
      lower = lower.indices.map(i => (if select then upper(i).last else data(i)) +: lower(i).dropRight(1)).toVector)
  private def empty(half: Int) = DelayUnit(half, Vector.fill(half, half)(-1), Vector.fill(half, half)(-1))

  test("short-delay schedule transposes 1000 frames with mixed gaps without stalling payload"):
    val random = new scala.util.Random(123456789L)
    val starts = (1 until 1000).scanLeft(0)((s, _) => s+4+random.nextInt(10)).toVector
    var first = empty(2); var lower = empty(1); var upper = empty(1)
    var phase = 0; var active = false; var delayedPhase = Vector(0,0)
    var inputFrame = -1; var outputFrame = -1; var checked = 0
    for cycle <- 0 until starts.last+10 do
      if inputFrame+1 < starts.size && starts(inputFrame+1) == cycle then inputFrame += 1
      if outputFrame+1 < starts.size && starts(outputFrame+1)+3 == cycle then outputFrame += 1
      val beat = cycle-starts(inputFrame)
      val input = Vector.tabulate(4)(lane => if beat < 4 then inputFrame*16+beat*4+lane else -1)
      val middle = first.output(input, (phase & 2) != 0)
      val select = delayedPhase.last != 0
      val output = lower.output(middle.take(2), select) ++ upper.output(middle.drop(2), select)
      // This value is captured by the output FF on this edge: SGen latency 4.
      if outputFrame >= 0 && cycle < starts(outputFrame)+7 then
        val outBeat = cycle-starts(outputFrame)-3
        assert(output == Vector.tabulate(4)(lane => outputFrame*16+lane*4+outBeat))
        checked += 4
      first = first.shift(input, (phase & 2) != 0)
      lower = lower.shift(middle.take(2), select)
      upper = upper.shift(middle.drop(2), select)
      delayedPhase = Vector(phase & 1, delayedPhase.head)
      if beat == 0 || active then
        active = phase != 3
        phase = (phase+1) % 4
    assert(checked == 16000)

  test("token mode initializes registered phase before payload without a phase override"):
    val (role, p) = BankedPermutationFixtures.permutations.head
    val tile = BankedPermutation.tiles(p, role).head
    val rtl = CommutatorPermutationVerilog.emit(tile, 60, frameControl = true)
    assert(rtl.contains("phase <= start_early ? 2'd0 : phase + 2'd1;"))
    assert(rtl.contains("child_select_delay <= {child_select_delay[0], phase[0]};"))
    assert(!rtl.contains("beat_phase"))
    assert(rtl.contains("phase_pair1 <= start_early ? 1'b0 : phase[1] ^ phase[0];"))
    assert(rtl.contains("lower_1_0 <= phase_pair1 ?"))
    assert(rtl.contains("lower_0_0 <= phase[1] ?"))

  test("a four-lane bank plan that is not a pure transpose is rejected"):
    val (role, p) = BankedPermutationFixtures.permutations.head
    val tile = BankedPermutation.tiles(p, role).head
    val changed = tile.copy(banks = tile.banks.map(_.map(w => w.copy(output = w.output ^ 2))))
    assertThrows[IllegalArgumentException](CommutatorPermutationVerilog.emit(changed, 60))
