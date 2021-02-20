module Compucolor2 (
           input        CLK100MHZ,

           input [15:0] SW,

           input        PS2_CLK,
           input        PS2_DATA,

           output       VGA_HS,
           output       VGA_VS,
           output [3:0] VGA_R,
           output [3:0] VGA_G,
           output [3:0] VGA_B
           );

   wire                 CLK_40MHZ;
   wire                 CLK_LOCKED;
   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign VGA_R = VGA_RED_FULL[7:4];
   assign VGA_G = VGA_GREEN_FULL[7:4];
   assign VGA_B = VGA_BLUE_FULL[7:4];

   ClockWiz40 u_ClockWiz40
     (.CLKIN_100MHZ(CLK100MHZ),
      .CLKOUT_40MHZ(CLK_40MHZ),
      .LOCKED(CLK_LOCKED),
      .reset(1'b0)
      );

   topEntity u_topEntity
     (.CLK_40MHZ(CLK_40MHZ),
      .RESET(!CLK_LOCKED),

      .SWITCHES(SW[7:0]),

      .PS2_CLK(PS2_CLK),
      .PS2_DATA(PS2_DATA),

      .VGA_HSYNC(VGA_HS),
      .VGA_VSYNC(VGA_VS),
      .VGA_RED(VGA_RED_FULL),
      .VGA_GREEN(VGA_GREEN_FULL),
      .VGA_BLUE(VGA_BLUE_FULL)
      );

endmodule
