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
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3814:39:
Value too wide for 32-bits expected in this context 64'sh100000003
: ... In instance topEntity
3814 | assign c$f5_case_alt = result_22[6-1-(64'sd0)*3 -: 3];
| ^
%Error:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3818:41:
Value too wide for 32-bits expected in this context 64'sh100000000
: ... In instance topEntity
3818 | assign c$f5_case_alt_0 = result_22[6-1-(64'sd1)*3 -: 3];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3814:35:
Bit extraction of var[5:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
3814 | assign c$f5_case_alt = result_22[6-1-(64'sd0)*3 -: 3];
| ^
... Use "/* verilator lint_off WIDTH */" and lint_on around source to disable
this message.
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3818:37:
Bit extraction of var[5:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
3818 | assign c$f5_case_alt_0 = result_22[6-1-(64'sd1)*3 -: 3];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3887:14:
Bit extraction of array[1023:0] requires 10 bit index, not 64 bits.
: ... In instance topEntity
3887 | f5 <= ROM[(wild_0)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3959:16:
Bit extraction of array[63:0] requires 6 bit index, not 64 bits.
: ... In instance topEntity
3959 | b2 <= ROM_0[(wild_1)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:3982:18:
Bit extraction of array[63:0] requires 6 bit index, not 64 bits.
: ... In instance topEntity
3982 | b1_2 <= ROM_1[(wild_2)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:4231:20:
Bit extraction of array[2047:0] requires 11 bit index, not 64 bits.
: ... In instance topEntity
4231 | result_31_RAM[(wild_3)] <= w0;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:4233:31:
Bit extraction of array[2047:0] requires 11 bit index, not 64 bits.
: ... In instance topEntity
4233 | result_31 <= result_31_RAM[(wild_4)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:4410:20:
Bit extraction of array[2047:0] requires 11 bit index, not 64 bits.
: ... In instance topEntity
4410 | result_33_RAM[(wild_5)] <= w0_0;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:4412:31:
Bit extraction of array[2047:0] requires 11 bit index, not 64 bits.
: ... In instance topEntity
4412 | result_33 <= result_33_RAM[(wild_6)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:5642:31:
Bit extraction of array[15:0] requires 4 bit index, not 64 bits.
: ... In instance topEntity
5642 | assign result_101 = vecArray[(wild_7)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6035:25:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6035 | assign b_29 = ((c$bv_2[(64'sd7)]) == (1'b1)) ? 1'b0 : 1'b1;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6099:21:
Bit extraction of array[16383:0] requires 14 bit index, not 64 bits.
: ... In instance topEntity
6099 | result_115_RAM[(wild_8)] <= w0_1;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6101:33:
Bit extraction of array[16383:0] requires 14 bit index, not 64 bits.
: ... In instance topEntity
6101 | result_115 <= result_115_RAM[(wild_9)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6174:27:
Bit extraction of array[16383:0] requires 14 bit index, not 64 bits.
: ... In instance topEntity
6174 | c$t_app_arg_2 <= ROM_2[(wild_10)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6412:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6412 | c$ds3_app_arg[(wild4)] = c$din;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6598:28:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6598 | assign b3 = (c$b1_app_arg[(64'sd2)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6600:30:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6600 | assign b2_0 = (c$b1_app_arg[(64'sd3)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6602:31:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6602 | assign b1_21 = (c$b1_app_arg[(64'sd4)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6618:31:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6618 | assign ds12 = ((c$b1_app_arg[(64'sd1)]) == (1'b1)) ? {{1'b0,
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6628:33:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6628 | assign ds11_0 = ((c$b1_app_arg[(64'sd0)]) == (1'b1)) ? result_147 :
{{1'b0,
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6713:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6713 | c$ds3_app_arg_0[(wild4_0)] = c$din_0;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:6987:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
6987 | c$ds2_app_arg[(64'sd0)] = c$din_1;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7004:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7004 | c$ds2_app_arg_0[(64'sd1)] = c$din_2;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7021:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7021 | c$ds2_app_arg_1[(64'sd3)] = c$din_3;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7034:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7034 | c$ds2_app_arg_2[(64'sd6)] = c$din_4;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7051:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7051 | c$ds2_app_arg_3[(64'sd7)] = c$din_5;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7139:8:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7139 | x16[(64'sd4)] = c$din_6;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7172:35:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7172 | c$controller_$j_$j1Out_app_arg[(64'sd5)] = c$din_7;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7267:17:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7267 | c$app_arg_45[(64'sd7)] = c$din_8;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7328:17:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7328 | c$app_arg_46[(64'sd6)] = c$din_9;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7389:17:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7389 | c$app_arg_47[(64'sd3)] = c$din_10;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7450:17:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7450 | c$app_arg_48[(64'sd1)] = c$din_11;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7511:17:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7511 | c$app_arg_49[(64'sd0)] = c$din_12;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7533:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7533 | c$karg_app_arg[(64'sd7)] = c$din_13;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7554:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7554 | c$karg_app_arg_0[(64'sd7)] = c$din_14;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7583:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
7583 | c$karg_app_arg_1[(64'sd2)] = c$din_15;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7655:25:
Operator ASSIGNW expects 32 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 64 bits.
: ... In instance topEntity
7655 | assign c$ds_app_arg_3 = $unsigned(c$ds_app_arg_4);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7660:25:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 32 bits.
: ... In instance topEntity
7660 | assign c$ds_app_arg_4 = $unsigned(c$w[31:0]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7663:15:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's VARREF
'cnt_13' generates 32 bits.
: ... In instance topEntity
7663 | assign x_23 = cnt_13;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7812:25:
Operator ASSIGNW expects 32 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 64 bits.
: ... In instance topEntity
7812 | assign c$ds_app_arg_6 = $unsigned(c$ds_app_arg_7);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7817:25:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 32 bits.
: ... In instance topEntity
7817 | assign c$ds_app_arg_7 = $unsigned(c$w_0[31:0]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7820:15:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's VARREF
'cnt_14' generates 32 bits.
: ... In instance topEntity
7820 | assign x_25 = cnt_14;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7851:34:
Operator ASSIGNW expects 32 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 64 bits.
: ... In instance topEntity
7851 | assign c$bitDuration2_case_alt = $unsigned(c$bitDuration2_app_arg);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7856:33:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 32 bits.
: ... In instance topEntity
7856 | assign c$bitDuration2_app_arg = $unsigned(c$w_1[31:0]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7859:24:
Operator ASSIGNW expects 32 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 64 bits.
: ... In instance topEntity
7859 | assign c$s1_case_alt = $unsigned(c$s1_app_arg);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7864:23:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 32 bits.
: ... In instance topEntity
7864 | assign c$s1_app_arg = $unsigned(c$w_2[31:0]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7867:15:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's VARREF
'c$bitDuration1_case_alt' generates 32 bits.
: ... In instance topEntity
7867 | assign \x# = c$bitDuration1_case_alt;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7869:34:
Operator ASSIGNW expects 32 bits on the Assign RHS, but Assign RHS's UNSIGNED
generates 64 bits.
: ... In instance topEntity
7869 | assign c$bitDuration1_case_alt = $unsigned((\x#_0 >> 64'sd1));
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:7871:17:
Operator ASSIGNW expects 64 bits on the Assign RHS, but Assign RHS's VARREF
'result_187' generates 32 bits.
: ... In instance topEntity
7871 | assign \x#_0 = result_187;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:8014:10:
Bit extraction of array[629759:0] requires 20 bit index, not 64 bits.
: ... In instance topEntity
8014 | RAM[(wild_15)] <= ds1_0[0:0];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:8016:22:
Bit extraction of array[629759:0] requires 20 bit index, not 64 bits.
: ... In instance topEntity
8016 | result_193 <= RAM[(wild_16)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:8223:29:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
8223 | assign b_31 = (parallelOut[(64'sd3)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:8241:29:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
8241 | assign b_33 = (parallelOut[(64'sd4)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10305:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10305 | c$ds4_app_arg_4[(64'sd5)] = c$din_16;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10314:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10314 | c$v_app_arg_2[(64'sd3)] = c$din_17;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10325:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10325 | c$v_app_arg_3[(64'sd1)] = c$din_18;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10341:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10341 | c$ds4_app_arg_5[(64'sd5)] = c$din_19;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10350:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10350 | c$v_app_arg_4[(64'sd3)] = c$din_20;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10361:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10361 | c$v_app_arg_5[(64'sd1)] = c$din_21;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10379:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10379 | c$ds4_app_arg_6[(64'sd5)] = c$din_22;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10388:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10388 | c$v_app_arg_6[(64'sd3)] = c$din_23;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10399:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10399 | c$v_app_arg_7[(64'sd1)] = c$din_24;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10415:20:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10415 | c$ds4_app_arg_7[(64'sd5)] = c$din_25;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10424:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10424 | c$v_app_arg_8[(64'sd3)] = c$din_26;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:10435:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
10435 | c$v_app_arg_9[(64'sd1)] = c$din_27;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11589:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11589 | c$app_arg_220[(wild_17)] = c$din_28;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11655:41:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11655 | 3'b010 : c$case_alt_133 = {(c$bv_3[(64'sd4)]) == (1'b1),
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11795:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11795 | c$v_app_arg_11[(64'sd1)] = c$din_29;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11804:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11804 | c$v_app_arg_12[(64'sd3)] = c$din_30;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11813:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11813 | c$eta1_app_arg_1[(64'sd5)] = c$din_31;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11821:24:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11821 | assign bv_1 = (c$bv_4[(64'sd0)]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11854:31:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11854 | assign bv_2 = (c$app_arg_227[(wild_17)]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11863:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11863 | c$eta1_app_arg_2[(64'sd5)] = c$din_32;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11872:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11872 | c$v_app_arg_13[(64'sd3)] = c$din_33;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11883:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11883 | c$v_app_arg_14[(64'sd1)] = c$din_34;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11941:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11941 | c$app_arg_229[(64'sd4)] = c$din_35;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11954:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11954 | c$eta1_app_arg_5[(64'sd5)] = c$din_36;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11965:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11965 | c$v_app_arg_15[(64'sd3)] = c$din_37;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:11974:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
11974 | c$v_app_arg_16[(64'sd1)] = c$din_38;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12020:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12020 | c$app_arg_232[(64'sd2)] = c$din_39;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12033:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12033 | c$eta1_app_arg_7[(64'sd5)] = c$din_40;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12044:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12044 | c$v_app_arg_17[(64'sd3)] = c$din_41;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12053:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12053 | c$v_app_arg_18[(64'sd1)] = c$din_42;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12215:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12215 | c$app_arg_235[(64'sd7)] = c$din_43;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12228:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12228 | c$eta1_app_arg_9[(64'sd5)] = c$din_44;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12239:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12239 | c$v_app_arg_19[(64'sd3)] = c$din_45;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12248:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12248 | c$v_app_arg_20[(64'sd1)] = c$din_46;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12252:30:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12252 | assign b2_2 = (c$b2_app_arg[(64'sd7)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12285:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12285 | c$app_arg_238[(64'sd6)] = c$din_47;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12298:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12298 | c$eta1_app_arg_11[(64'sd5)] = c$din_48;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12309:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12309 | c$v_app_arg_21[(64'sd3)] = c$din_49;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12318:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12318 | c$v_app_arg_22[(64'sd1)] = c$din_50;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12334:41:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12334 | 3'b010 : c$case_alt_138 = {(c$bv_8[(64'sd4)]) == (1'b1),
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12460:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12460 | c$v_app_arg_23[(64'sd1)] = c$din_51;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12469:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12469 | c$v_app_arg_24[(64'sd3)] = c$din_52;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12478:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12478 | c$eta1_app_arg_13[(64'sd5)] = c$din_53;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12486:24:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12486 | assign bv_3 = (c$bv_9[(wild_19)]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12572:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12572 | c$v_app_arg_25[(64'sd1)] = c$din_54;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12581:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12581 | c$v_app_arg_26[(64'sd3)] = c$din_55;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12590:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12590 | c$eta1_app_arg_15[(64'sd5)] = c$din_56;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12600:25:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12600 | assign bv_4 = (c$bv_10[(wild_20)]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12689:21:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12689 | c$ds2_app_arg_22[(64'sd5)] = c$din_57;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12698:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12698 | c$v_app_arg_27[(64'sd3)] = c$din_58;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12709:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12709 | c$v_app_arg_28[(64'sd1)] = c$din_59;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12772:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12772 | c$app_arg_250[(64'sd4)] = c$din_60;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12785:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12785 | c$eta1_app_arg_17[(64'sd5)] = c$din_61;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12796:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12796 | c$v_app_arg_29[(64'sd3)] = c$din_62;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12805:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12805 | c$v_app_arg_30[(64'sd1)] = c$din_63;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12853:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12853 | c$app_arg_253[(64'sd2)] = c$din_64;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12866:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12866 | c$eta1_app_arg_19[(64'sd5)] = c$din_65;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12877:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12877 | c$v_app_arg_31[(64'sd3)] = c$din_66;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:12886:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
12886 | c$v_app_arg_32[(64'sd1)] = c$din_67;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13048:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13048 | c$app_arg_256[(64'sd7)] = c$din_68;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13061:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13061 | c$eta1_app_arg_21[(64'sd5)] = c$din_69;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13072:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13072 | c$v_app_arg_33[(64'sd3)] = c$din_70;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13081:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13081 | c$v_app_arg_34[(64'sd1)] = c$din_71;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13085:32:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13085 | assign b2_4 = (c$b2_app_arg_0[(64'sd7)]) == (1'b1);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13118:18:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13118 | c$app_arg_259[(64'sd6)] = c$din_72;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13131:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13131 | c$eta1_app_arg_23[(64'sd5)] = c$din_73;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13142:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13142 | c$v_app_arg_35[(64'sd3)] = c$din_74;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13151:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13151 | c$v_app_arg_36[(64'sd1)] = c$din_75;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13167:42:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13167 | 3'b010 : c$case_alt_146 = {(c$bv_13[(64'sd4)]) == (1'b1),
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13303:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13303 | c$v_app_arg_37[(64'sd1)] = c$din_76;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13312:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13312 | c$v_app_arg_38[(64'sd3)] = c$din_77;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13321:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13321 | c$eta1_app_arg_25[(64'sd5)] = c$din_78;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13329:25:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13329 | assign bv_5 = (c$bv_14[(64'sd0)]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13342:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13342 | c$v_app_arg_39[(64'sd1)] = c$din_79;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13351:19:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13351 | c$v_app_arg_40[(64'sd3)] = c$din_80;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13360:22:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13360 | c$eta1_app_arg_27[(64'sd5)] = c$din_81;
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13368:25:
Bit extraction of var[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13368 | assign bv_6 = (c$bv_15[(64'sd4)]);
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13411:38:
Bit extraction of array[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13411 | assign c$ds4_app_arg_8 = vecArray_0[(v_34)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13428:38:
Bit extraction of array[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13428 | assign c$ds4_app_arg_9 = vecArray_1[(v_35)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13447:39:
Bit extraction of array[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13447 | assign c$ds4_app_arg_10 = vecArray_2[(v_36)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13464:39:
Bit extraction of array[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13464 | assign c$ds4_app_arg_11 = vecArray_3[(v_37)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13483:39:
Bit extraction of array[7:0] requires 3 bit index, not 64 bits.
: ... In instance topEntity
13483 | assign c$ds2_app_arg_29 = vecArray_4[(v_38)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:13510:38:
Bit extraction of array[16:0] requires 5 bit index, not 64 bits.
: ... In instance topEntity
13510 | assign c$ds_case_scrut = vecArray_5[(wild_22)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:17928:33:
Bit extraction of array[255:0] requires 8 bit index, not 64 bits.
: ... In instance topEntity
17928 | assign c$ds_app_arg_22 = ROM_3[(wild1)];
| ^
%Warning-WIDTH:
/compucolor-book/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/compucolor2-verilator/_clashilator/clash-syn/Hardware.Compucolor2.topEntity/topEntity.v:19236:44:
Bit extraction of array[255:0] requires 8 bit index, not 64 bits.
: ... In instance topEntity
19236 | assign c$addressingOut_case_scrut = ROM_4[(wild14)];
| ^
%Error: Exiting due to 2 error(s)
```
