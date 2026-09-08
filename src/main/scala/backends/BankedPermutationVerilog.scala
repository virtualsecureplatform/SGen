package backends

import transforms.perm.BankedTile

object BankedPermutationVerilog:
  def emit(tile: BankedTile, width: Int, localAdmission: Boolean = false): String =
    require(width > 0)
    val admission = if localAdmission then
      """  // BANKED_CONTROL variant=banked_local_control token_lead=1
        |  (* keep = "TRUE", dont_touch = "TRUE", shreg_extract = "no" *) reg local_start;
        |  always @(posedge clk) begin
        |    if (reset) local_start <= 0;
        |    else local_start <= start_early;
        |  end
        |  wire start = local_start;
        |""".stripMargin
    else ""
    val assertions = if localAdmission then
      """  // synthesis translate_off
        |  always @(posedge clk) if (!reset) begin
        |    if (!write_active && write_phase != 0) $fatal(1, "idle phase must be zero");
        |    if (start && (write_active || write_phase != 0)) $fatal(1, "frame admission must be idle at phase zero");
        |  end
        |  // synthesis translate_on
        |""".stripMargin
    else ""
    def pick(index: String, values: Seq[String]): String =
      values.dropRight(1).zipWithIndex.map((v, i) => s"($index == 2'd$i) ? $v : ").mkString + values.last
    def input(lane: Int) = s"data_in[${(lane+1)*width-1}:${lane*width}]"
    val memories = tile.banks.indices.map { bank =>
      val words = tile.banks(bank)
      val writeData = pick("write_beat", words.sortBy(_.writeBeat).map(w => input(tile.inputs.indexOf(w.input % 128))))
      val readAddress = pick("read_phase", words.sortBy(_.readBeat).map(w => s"2'd${w.writeBeat}"))
      s"""  (* ram_style = "block" *) reg [${width-1}:0] bank_$bank [0:7];
         |  reg [${width-1}:0] bank_${bank}_q;
         |  wire [1:0] bank_${bank}_read_address = $readAddress;
         |  always @(posedge clk) begin
         |    if (write_enable) bank_$bank[{write_page,write_beat}] <= $writeData;
         |    if (read_active) bank_${bank}_q <= bank_$bank[{read_page,bank_${bank}_read_address}];
         |  end
         |""".stripMargin
    }.mkString
    val outputReads = tile.outputs.zipWithIndex.map { (lane, local) =>
      val bankAtBeat = Vector.tabulate(4) { beat =>
        val bank = tile.banks.indexWhere(_.exists(w => w.output % 128 == lane && w.readBeat == beat))
        require(bank >= 0)
        s"bank_${bank}_q"
      }
      s"    data_out[${(local+1)*width-1}:${local*width}] <= ${pick("read_phase_q", bankAtBeat)};"
    }.mkString("\n")
    val manifest = tile.banks.zipWithIndex.flatMap((bank, b) => bank.map(w =>
      s"// BANKED_WORD role=${tile.role} tile=${tile.ordinal} bank=$b input=${w.input} output=${w.output} address=${w.writeBeat}" )).mkString("\n")
    s"""// BANKED_TILE role=${tile.role} tile=${tile.ordinal} lanes_in=${tile.inputs.mkString(",")} lanes_out=${tile.outputs.mkString(",")} width=$width latency=6
       |$manifest
       |(* keep_hierarchy = "yes" *) module ${tile.moduleName}${if localAdmission then "_Local" else ""}(
       |  input clk, input reset, input ${if localAdmission then "start_early" else "start"},
       |  input [${4*width-1}:0] data_in, output reg [${4*width-1}:0] data_out
       |);
       |${admission}  (* keep = "TRUE", dont_touch = "TRUE", shreg_extract = "no" *) reg [1:0] write_phase, read_phase;
       |  (* keep = "TRUE", dont_touch = "TRUE" *) reg write_page, read_page;
       |  reg write_active, read_active;
       |  reg [1:0] read_phase_q;
       |  wire write_enable = !reset && (start || write_active);
       |  wire [1:0] write_beat = ${if localAdmission then "write_phase" else "start ? 2'd0 : write_phase"};
       |$memories
       |  always @(posedge clk) begin
       |    if (reset) begin
       |      write_phase <= 0; write_page <= 0; write_active <= 0;
       |      read_phase <= 0; read_page <= 0; read_active <= 0;
       |    end else begin
       |      if (write_enable) begin
       |        write_phase <= write_beat + 2'd1;
       |        write_active <= write_beat != 2'd3;
       |        if (write_beat == 2'd3) begin
       |          write_page <= ~write_page;
       |        end
       |      end
       |      if (read_active) begin
       |        read_phase <= read_phase + 2'd1;
       |        if (read_phase == 2'd3) read_active <= 0;
       |      end
       |      if (write_enable && write_beat == 2'd3) begin
       |        read_page <= write_page; read_phase <= 0; read_active <= 1;
       |      end
       |    end
       |    read_phase_q <= read_phase;
       |$outputReads
       |  end
       |${assertions}endmodule
       |""".stripMargin
