package backends

import backends.Verilog.*
import ir.rtl.RAMControl
import ir.rtl.hardwaretype.{HW, FixedPoint}
import maths.linalg.Matrix
import maths.fields.F2
import transforms.perm.{BankedPermutation, LinearPerm}
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets.UTF_8

object BankedPermutationFixtures:
  val permutations = Vector(
    "input" -> LinearPerm.Rmat(3,9),
    "final_input" -> LinearPerm.Qmat(9,3,0),
    "output" -> LinearPerm.Lmat(3,9))

class BankedPermutationTest extends AnyFunSuite:
  for (role, p) <- BankedPermutationFixtures.permutations do
    test(s"$role maps every coordinate once with four conflict-free banks"):
      val tiles = BankedPermutation.tiles(p, role)
      assert(tiles.size == 32)
      val words = tiles.flatMap(_.banks.flatten)
      assert(words.map(_.input).sorted == (0 until 512))
      assert(words.map(_.output).sorted == (0 until 512))
      words.foreach(w => assert(w.output == LinearPerm.permute(p, w.input)))
      assert(tiles == BankedPermutation.tiles(p, role))
      tiles.foreach { tile =>
        for beat <- 0 until 4 do
          assert(tile.banks.map(_.find(_.writeBeat == beat).get.input % 128).sorted == tile.inputs)
          assert(tile.banks.map(_.find(_.readBeat == beat).get.output % 128).sorted == tile.outputs)
        val rtl = BankedPermutationVerilog.emit(tile, 60)
        assert("ram_style".r.findAllIn(rtl).size == 4)
        assert(!rtl.contains("bank_0 <= 0"))
      }
    test(s"$role stream retains six-cycle latency and four-cycle interval"):
      given HW[Double] = FixedPoint(32, 0)
      val stream = BankedPermutation[Double](p, role).stream(7, RAMControl.Single)
      assert(stream.latency == 6 && stream.minGap == 0 && stream.nextAt == 0)
      assert(stream.toVerilogWithMuxControlBudget(0).contains("BANKED_TILE"))
  test("unsupported tile shape is rejected"):
    assertThrows[IllegalArgumentException](BankedPermutation.tiles(Matrix.identity[F2](9), "identity"))

  for (role, p) <- BankedPermutationFixtures.permutations do
    test(s"$role local admission changes token lead but not data latency"):
      given HW[Double] = FixedPoint(32, 0)
      val stream = BankedPermutation[Double](p, role, true).stream(7, RAMControl.Single)
      assert(stream.latency == 6 && stream.minGap == 0 && stream.nextAt == -1)
      val rtl = stream.toVerilogWithMuxControlBudget(0)
      assert(rtl.contains("reg local_start"))
      assert(rtl.contains("write_beat = write_phase"))
      assert(!rtl.contains("start ? 2'd0"))

  test("all reachable legal admission states have zero idle phase"):
    var reached = Set((0, false))
    var previous = Set.empty[(Int, Boolean)]
    while reached != previous do
      previous = reached
      for (phase, active) <- previous; reset <- Seq(false, true); start <- Seq(false, true)
          if !start || (!active && phase == 0) do
        assert(active || phase == 0)
        val oldBeat = if start then 0 else phase
        assert(oldBeat == phase)
        val next = if reset then (0, false)
          else if start || active then ((phase + 1) % 4, phase != 3)
          else (phase, active)
        reached += next
    assert(reached == Set((0, false), (1, true), (2, true), (3, true)))

  for (role, p) <- BankedPermutationFixtures.permutations do
    test(s"$role commutator is a pure transpose with registered four-cycle output"):
      given HW[Double] = FixedPoint(32, 0)
      val transform = BankedPermutation[Double](p, role, commutator = true)
      val stream = transform.stream(7, RAMControl.Single)
      assert(stream.latency == 4 && stream.nextAt == -1 && stream.minGap == 0)
      val rtl = stream.toVerilogWithMuxControlBudget(0)
      assert("COMMUTATOR_TILE".r.findAllIn(rtl).size == 32)
      assert(!rtl.contains("ram_style"))
      assert(!rtl.contains("bank_0"))
      assert(rtl.contains("output_register_words=4"))

/** Exhaustive-coordinate RTL fixture, run with XSim by the parent repository. */
object EmitBankedPermutationTests:
  def main(args: Array[String]): Unit =
    val directory = Paths.get(args(0)); Files.createDirectories(directory)
    val commutator = args.drop(1).contains("commutator")
    val local = args.drop(1).contains("local") || commutator
    given HW[Double] = FixedPoint(32, 0)
    val designs = BankedPermutationFixtures.permutations.map { (role, p) =>
      BankedPermutation[Double](p, role).stream(7, RAMControl.Single).toVerilogWithMuxControlBudget(0)
        .replace("module main(", s"module test_$role(")
    }
    val localDesigns = if !local then Vector.empty else BankedPermutationFixtures.permutations.map { (role, p) =>
      BankedPermutation[Double](p, role, true, commutator).stream(7, RAMControl.Single).toVerilogWithMuxControlBudget(0)
        .replace("module main(", s"module local_$role(")
    }
    Files.write(directory.resolve("permutations.v"), (designs ++ localDesigns).mkString("\n").getBytes(UTF_8))
    val declarations = BankedPermutationFixtures.permutations.map { (role, _) =>
      s"wire next_$role; wire [4095:0] out_$role; test_$role dut_$role(.clk(clk),.reset(reset),.next(start),.next_out(next_$role)," +
        (0 until 128).map(i => s".i$i(data[${32*i+31}:${32*i}]),.o$i(out_$role[${32*i+31}:${32*i}])").mkString(",") + ");"
    }.mkString("\n")
    val localDeclarations = if !local then "" else BankedPermutationFixtures.permutations.map { (role, _) =>
      val prefix = if commutator then "raw" else "local"
      val alignment = if !commutator then "" else s"""
        |reg [4095:0] delay_out_$role, local_out_$role; reg delay_next_$role=0, local_next_$role=0;
        |always @(posedge clk) begin
        |  delay_out_$role <= raw_out_$role; local_out_$role <= delay_out_$role;
        |  if (reset) begin delay_next_$role<=0; local_next_$role<=0; end
        |  else begin delay_next_$role<=raw_next_$role; local_next_$role<=delay_next_$role; end
        |end
        |always @(negedge clk) if (!reset && local_next_$role !== next_$role) $$fatal(1,"aligned commutator token mismatch $role");
        |""".stripMargin
      s"wire ${prefix}_next_$role; wire [4095:0] ${prefix}_out_$role; local_$role local_dut_$role(.clk(clk),.reset(reset),.next(early),.next_out(${prefix}_next_$role)," +
        (0 until 128).map(i => s".i$i(data[${32*i+31}:${32*i}]),.o$i(${prefix}_out_$role[${32*i+31}:${32*i}])").mkString(",") + ");" + alignment
    }.mkString("\n")
    val equivalenceChecks = if !local || commutator then "" else BankedPermutationFixtures.permutations.flatMap { (role, p) =>
      val token = s"always @(negedge clk) if (!reset && local_next_$role !== next_$role) $$fatal(1,\"local token mismatch $role\");"
      Vector(token) ++ BankedPermutation.tiles(p, role).flatMap { tile =>
        val ref = s"dut_$role.tile_${role}_${tile.ordinal}"
        val dut = s"local_dut_$role.tile_${role}_${tile.ordinal}"
        Vector(s"always @(posedge clk) if (!reset) begin if ($ref.write_enable !== $dut.write_enable || $ref.read_active !== $dut.read_active) $$fatal(1,\"enable mismatch\"); end") ++
          (0 until 4).map { bank =>
            s"always @(posedge clk) if (!reset) begin " +
              s"if ($dut.write_enable && {$ref.write_page,$ref.write_beat,$ref.data_in} !== {$dut.write_page,$dut.write_beat,$dut.data_in}) $$fatal(1,\"write transaction mismatch\"); " +
              s"if ($dut.read_active && {$ref.read_page,$ref.bank_${bank}_read_address} !== {$dut.read_page,$dut.bank_${bank}_read_address}) $$fatal(1,\"read transaction mismatch\"); " +
              s"if ($dut.write_enable && $dut.read_active && $dut.write_page == $dut.read_page && $dut.write_beat == $dut.bank_${bank}_read_address) $$fatal(1,\"local bank collision\"); end"
          }
      }
    }.mkString("\n")
    val starts = if !commutator then Vector(0,4,8,12,24,28,42,46,50,54)
      else (0 to 9).scanLeft(0)((s, gap) => s+4+gap).toVector ++ Vector(120,124,128)
    val collisionChecks = BankedPermutationFixtures.permutations.flatMap { (role,p) =>
      BankedPermutation.tiles(p,role).flatMap { tile =>
        val inst = s"dut_$role.tile_${role}_${tile.ordinal}"
        (0 until 4).map { bank =>
          s"always @(posedge clk) if (!reset && $inst.write_enable && $inst.read_active && $inst.write_page == $inst.read_page && $inst.write_beat == $inst.bank_${bank}_read_address) $$fatal(1,\"bank collision $role/${tile.ordinal}/$bank\");"
        }
      }
    }.mkString("\n")
    val stimulus = (-1 until (starts.last+18)).map { cycle =>
      val frame = starts.indexWhere(s => cycle >= s && cycle < s+4)
      val beat = if frame >= 0 then cycle-starts(frame) else 0
      val input = if frame < 0 then "data = 0;" else (0 until 128).map(lane =>
        s"data[${32*lane+31}:${32*lane}] = seed + 32'd${frame*512+beat*128+lane};").mkString("\n")
      val outputFrame = starts.indexWhere(s => cycle >= s+5 && cycle < s+9)
      val checks = BankedPermutationFixtures.permutations.map { (role,p) =>
        val token = if starts.exists(s => cycle == s+5) then 1 else 0
        val values = if outputFrame < 0 then "" else (0 until 128).map { lane =>
          val output = (cycle-starts(outputFrame)-5)*128+lane
          val input = LinearPerm.permute(p.inverse, output)
          s"if (out_$role[${32*lane+31}:${32*lane}] !== seed + 32'd${outputFrame*512+input}) $$fatal(1,\"$role cycle=$cycle lane=$lane payload\");"
        }.mkString("\n")
        val localCheck = if !local || outputFrame < 0 then "" else s"if (local_out_$role !== out_$role) $$fatal(1,\"local payload mismatch $role cycle=$cycle\");"
        s"if (next_$role !== 1'b$token) $$fatal(1,\"$role cycle=$cycle token\");\n$values\n$localCheck"
      }.mkString("\n")
      s"@(negedge clk); early = ${if starts.contains(cycle+1) then 1 else 0}; start = ${if starts.contains(cycle) then 1 else 0}; $input\n@(posedge clk); #1;\n$checks"
    }.mkString("\n")
    val tb = s"""module banked_test;
       |reg clk=0,reset=1,start=0,early=0; reg [4095:0] data=0; integer seed,abort_phase;
       |always #5 clk=~clk;
       |$declarations
       |$localDeclarations
       |$collisionChecks
       |$equivalenceChecks
       |task run_frames;
       |begin
       |$stimulus
       |end
       |endtask
       |initial begin
       |repeat(8) @(negedge clk); reset=0; seed=0; run_frames;
       |// Abort an in-flight frame and reset without resetting payload RAM.
       |for (abort_phase=0;abort_phase<8;abort_phase=abort_phase+1) begin
       |@(negedge clk); early=1;
       |@(negedge clk); early=0; start=1; data=4096'habcdef;
       |if (abort_phase==0) reset=1;
       |@(negedge clk); start=0;
       |repeat(abort_phase) @(negedge clk);
       |reset=1; early=0;
       |repeat(8) @(negedge clk); reset=0; seed=32'h80000000+abort_phase*65536; run_frames;
       |end
       |$$display("BANKED_PERMUTATION_RTL_PASS"); $$finish;
       |end
       |endmodule
       |""".stripMargin
    Files.write(directory.resolve("banked_test.sv"), tb.getBytes(UTF_8))
