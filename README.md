
x266 - open H.266 codec reference implementation
==========================================

x266 is an open source highly optimal software/hardware co-design architecture implementation of the next generation H.266 video codec.

It is demonstration research of industrialized implement video coding H.266. as soon as H.266 specification release, I will publishing my industrialize H.266 codec.

_x266 based on Software/Hardware Cooperative concept and custom RISC-V processor with audio/video/image/deep_learning SIMD extension._


Building
========

- RISC-V<br>
    * Build:<br>
      * build risv<br>
      * build rtl_risv<br>
    * Verify:<br>
      * ./run_asm.sh risv<br>
      * ./run_bmark.sh risv<br>

Performance
========

|   Ver   |  FPGA / ASIC   |   LUT / Area   |   MHz  |
| :-----: |     :---:      |       ---:     |   ---: |
|  0.3.1  |     XC7Z030    |      2,203     |  159.0 |
|  0.3.1  |     XCZU9EG    |      2,207     |  245.3 |
|  0.3    |     130 nm     |      0.64 mm^2 |  400.0 |

|   Case    |   rdcycle  |  rdinstret |   CPI  |
| :-------: |  --------: |   ------:  |   ---: |
|  median   |     7,334  |     5,256  |   1.40 |
|  multiply |    43,514  |    26,911  |   1.62 |
|  qsort    |   213,491  |   158,322  |   1.35 |
|  towers   |     6,821  |     5,293  |   1.29 |
|  vvadd    |     4,511  |     2,709  |   1.67 |
|  rsort    |   305,357  |   177,314  |   1.72 |
|  spmv     | 1,039,346  |   776,241  |   1.34 |
|   Total   | 1,620,374  | 1,152,046  |   1.41 |


Prebuilt
=================

- C/C++ interface code can be obtained from /src_c<br>
- Verilog code can be obtained from /src_ver<br>

Note
=================
- The x266's RISC-V NON-Standard Vector Extension ONLY compile and disassembly by x266 project modified llvm suite. You may build bu yourself or please contract to get prebuilt Centos-6 binary package.


License
=======

x266 is distributed under the terms of the Private/Education ONLY License.
See COPYRIGHT for more details.

See `LICENSE.TXT` for more details.

Creator on November 2015<br>
Copyright (c) 2015-2017 Min Chen<br>
Contact: Min Chen <chenm003@{163, gmail}.com><br>
