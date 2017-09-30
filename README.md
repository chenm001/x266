
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
|  0.3    |     XC7Z030    |      2,076     |  152.2 |
|  0.3    |     XCZU9EG    |      1,956     |  227.3 |
|  0.3    |     130 nm     |      0.64 mm^2 |  400.0 |


Prebuilt
=================

- C/C++ interface code can be obtained from /src_c<br>
- Verilog code can be obtained from /src_ver<br>


License
=======

x266 is distributed under the terms of the Private/Education ONLY License.
See COPYRIGHT for more details.

See `LICENSE.TXT` for more details.

Creator on November 2015<br>
Copyright (c) 2015-2017 Min Chen<br>
Contact: Min Chen <chenm003@{163, gmail}.com><br>
