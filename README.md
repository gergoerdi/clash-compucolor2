# Clash issue 1934

https://github.com/clash-lang/clash-compiler/issues/1934

This works:

```
stack build --resolver=clash-good.yaml
```

But this fails:

```
stack build --resolver=clash-bad.yaml
```

With:

```
%Error:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:17:22:
Value too wide for 32-bits expected in this context 64'sh100000000
: ... In instance topEntity
17 | assign VGA = f2[8-1-(64'sd0)*8 -: 8];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:17:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
17 | assign VGA = f2[8-1-(64'sd0)*8 -: 8];
| ^
... Use "/* verilator lint_off WIDTH */" and lint_on around source to disable
this message.
%Error: Exiting due to 1 error(s)
```


Clash output with "good":

```
/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.2. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input  CLK // clock
    , input  RESET // reset

      // Outputs
    , output wire [7:0] VGA
    );
  wire [7:0] f2;
  reg [7:0] \$s!!_attr  = 8'd0;

  assign VGA = f2[7:0];

  assign f2 = \$s!!_attr ;

  // register begin
  always @(posedge CLK or  posedge  RESET) begin : _$s_attr_register
    if ( RESET) begin
      \$s!!_attr  <= 8'd0;
    end else begin
      \$s!!_attr  <= \$s!!_attr ;
    end
  end
  // register end


endmodule
```

Clash output with "bad":

```
/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.2. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input  CLK // clock
    , input  RESET // reset

      // Outputs
    , output wire [7:0] VGA
    );
  wire [7:0] f2;
  reg [7:0] \$s!!_attr  = 8'd0;

  // index lit begin
  assign VGA = f2[8-1-(64'sd0)*8 -: 8];
  // index lit end

  assign f2 = \$s!!_attr ;

  // register begin
  always @(posedge CLK or  posedge  RESET) begin : _$s_attr_register
    if ( RESET) begin
      \$s!!_attr  <= 8'd0;
    end else begin
      \$s!!_attr  <= \$s!!_attr ;
    end
  end
  // register end


endmodule
```
