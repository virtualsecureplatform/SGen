package backends

/** Exact full-rate square lane/time transpose used at FPT boundaries.
  *
  * The recursive SwitchTransposeUnit network remains available as a primitive,
  * but this two-bank implementation is used by the FPT wrapper while its
  * hardware lane-order contract is being characterized. It preserves the
  * required natural (cycle,lane) transpose for consecutive frames.
  */
object FptSquareTransposeVerilog:
  def emit(logSize: Int, dataWidth: Int, top: String): String =
    require(logSize > 0 && dataWidth > 0 && top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val width = 1 << logSize
    val capture0 = Vector.tabulate(width)(lane => s"storage_0[capture_count*$width+$lane]<=i$lane;").mkString(" ")
    val capture1 = Vector.tabulate(width)(lane => s"storage_1[capture_count*$width+$lane]<=i$lane;").mkString(" ")
    val output0 = Vector.tabulate(width)(lane => s"o$lane<=storage_0[$lane*$width+output_count];").mkString(" ")
    val output1 = Vector.tabulate(width)(lane => s"o$lane<=storage_1[$lane*$width+output_count];").mkString(" ")
    val inputs = Vector.tabulate(width)(lane => s"input [${dataWidth - 1}:0] i$lane")
    val outputs = Vector.tabulate(width)(lane => s"output reg [${dataWidth - 1}:0] o$lane")
    s"""// Exact two-bank square transpose: $width cycles x $width lanes.
       |module $top(input clk,input reset,input next,output reg next_out,
       |${(inputs ++ outputs).map("  " + _).mkString(",\n")}
       |);
       |  localparam [1:0] FREE=0,CAPTURE=1,QUEUED=2,OUTPUT=3;
       |  reg [1:0] state[0:1];reg capture_active,capture_buffer,output_active,output_buffer;integer capture_count,output_count,index;
       |  reg [${dataWidth - 1}:0] storage_0[0:${width * width - 1}],storage_1[0:${width * width - 1}];
       |  always @(posedge clk)begin
       |    if(reset)begin capture_active<=0;capture_buffer<=0;output_active<=0;output_buffer<=0;capture_count<=0;output_count<=0;next_out<=0;state[0]<=FREE;state[1]<=FREE;for(index=0;index<$width*$width;index=index+1)begin storage_0[index]<=0;storage_1[index]<=0;end end
       |    else begin
       |      next_out<=0;
       |      if((next&&!capture_active)||capture_active)begin
       |        if(!capture_active)begin if(state[0]==FREE)begin $capture0 state[0]<=CAPTURE;capture_buffer<=0;end else begin $capture1 state[1]<=CAPTURE;capture_buffer<=1;end capture_active<=1;end
       |        else if(capture_buffer==0)begin $capture0 end else begin $capture1 end
       |        if(capture_count==$width-1)begin if(capture_active?capture_buffer:(state[0]==FREE?0:1))state[1]<=QUEUED;else state[0]<=QUEUED;capture_count<=0;capture_active<=0;end else capture_count<=capture_count+1;
       |      end
       |      if(!output_active)begin
       |        if(state[0]==QUEUED)begin state[0]<=OUTPUT;output_buffer<=0;output_active<=1;output_count<=1;next_out<=1;$output0 end
       |        else if(state[1]==QUEUED)begin state[1]<=OUTPUT;output_buffer<=1;output_active<=1;output_count<=1;next_out<=1;$output1 end
       |      end else begin
       |        if(output_buffer==0)begin $output0 end else begin $output1 end
       |        if(output_count==$width-1)begin if(output_buffer==0)state[0]<=FREE;else state[1]<=FREE;output_count<=0;output_active<=0;end else output_count<=output_count+1;
       |      end
       |    end
       |  end
       |endmodule
       |""".stripMargin
