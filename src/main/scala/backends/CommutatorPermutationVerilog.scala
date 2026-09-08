package backends

import transforms.perm.BankedTile

/** Four-lane, four-beat transpose with continuous, unreset short delay lines. */
object CommutatorPermutationVerilog:
  def emit(tile: BankedTile, width: Int): String =
    require(width > 0)
    require(tile.banks.flatten.forall(w => w.readBeat == tile.inputs.indexOf(w.input % 128) &&
      tile.outputs.indexOf(w.output % 128) == w.writeBeat), "not a pure four-by-four transpose")
    def word(signal: String, lane: Int) = s"$signal[${lane*width} +: $width]"
    val registers = (for pair <- 0 until 2; delay <- 0 until 2 yield
      s"reg [${width-1}:0] upper_${pair}_$delay, lower_${pair}_$delay;").mkString("\n  ") +
      (0 until 2).map(pair => s"\n  reg [${width-1}:0] child_upper_$pair, child_lower_$pair;").mkString
    val middle = (0 until 2).flatMap(pair => Seq(
      s"assign ${word("middle",pair)} = lower_${pair}_1;",
      s"assign ${word("middle",pair+2)} = phase[1] ? ${word("data_in",pair)} : upper_${pair}_1;"
    )).mkString("\n  ")
    val shifts = (0 until 2).flatMap(pair => Seq(
      s"upper_${pair}_0 <= ${word("data_in",pair+2)};",
      s"upper_${pair}_1 <= upper_${pair}_0;",
      s"lower_${pair}_0 <= phase[1] ? upper_${pair}_1 : ${word("data_in",pair)};",
      s"lower_${pair}_1 <= lower_${pair}_0;",
      s"child_upper_$pair <= ${word("middle",2*pair+1)};",
      s"child_lower_$pair <= child_select_delay[1] ? child_upper_$pair : ${word("middle",2*pair)};",
      s"${word("data_out",2*pair)} <= child_lower_$pair;",
      s"${word("data_out",2*pair+1)} <= child_select_delay[1] ? ${word("middle",2*pair)} : child_upper_$pair;"
    )).mkString("\n    ")
    val mapping = tile.banks.flatten.sortBy(_.input).map(w =>
      s"// COMMUTATOR_WORD role=${tile.role} tile=${tile.ordinal} input=${w.input} output=${w.output}").mkString("\n")
    s"""// COMMUTATOR_TILE role=${tile.role} tile=${tile.ordinal} lanes_in=${tile.inputs.mkString(",")} lanes_out=${tile.outputs.mkString(",")} width=$width latency=4 token_lead=1 delays=2,1 delay_words=12 output_register_words=4
       |$mapping
       |(* keep_hierarchy = "yes" *) module SGenCommutatorPermutation_${tile.role}_${tile.ordinal}(
       |  input clk, input reset, input start_early,
       |  input [${4*width-1}:0] data_in, output reg [${4*width-1}:0] data_out
       |);
       |  (* keep = "TRUE", dont_touch = "TRUE", shreg_extract = "no" *) reg local_start;
       |  (* keep = "TRUE", dont_touch = "TRUE", shreg_extract = "no" *) reg [1:0] phase, child_select_delay;
       |  reg active;
       |  $registers
       |  wire [${4*width-1}:0] middle;
       |  $middle
       |  always @(posedge clk) begin
       |    if (reset) begin
       |      local_start <= 0; phase <= 0; child_select_delay <= 0; active <= 0;
       |    end else begin
       |      local_start <= start_early;
       |      child_select_delay <= {child_select_delay[0], phase[0]};
       |      if (local_start || active) begin
       |        phase <= phase + 2'd1;
       |        active <= phase != 2'd3;
       |      end
       |    end
       |  end
       |  // Payload must keep shifting during gaps; only control is reset.
       |  always @(posedge clk) begin
       |    $shifts
       |  end
       |  // synthesis translate_off
       |  always @(posedge clk) if (!reset) begin
       |    if (!active && phase != 0) $$fatal(1, "idle phase must be zero");
       |    if (local_start && (active || phase != 0)) $$fatal(1, "illegal frame admission");
       |  end
       |  // synthesis translate_on
       |endmodule
       |""".stripMargin
