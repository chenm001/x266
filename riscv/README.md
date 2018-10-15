
HW Acceleate Components (HAC)
==========================================
<br>
An embedded hardare accelerated modules for Audio, Video and Deep Learning.<br> 
<br>

Roadmap
========
<br>
The system design as an embedeed control based on RISC-V, it design with features:<br>
Instruction Cache
On-chip SRAM (shared by Data/Instruction)


Progressing
========

|   Module          |   LUT / Area   |   MHz  |  Status  |
|  :-----:          |   ---------:   |  ---:  |  :-----: |
| RISC-V            |                |        |          |
| Cache (Inst)      |                |        |          |
| Register Rename   |                |        |          |
| OOO               |                |        |          |
| Vector Processor  |                |        |          |


[TBD]

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
|         |     130 nm     |      _.__ mm^2 |  ___._ |


|   Case    |   rdcycle  |  rdinstret |   CPI  |
| :-------: |  --------: |   ------:  |   ---: |
|  median   |      ,     |      ,     |    .   |
|  multiply |      ,     |      ,     |    .   |
|  qsort    |      ,     |      ,     |    .   |
|  towers   |      ,     |      ,     |    .   |
|  vvadd    |      ,     |      ,     |    .   |
|  rsort    |      ,     |      ,     |    .   |
|  spmv     |      ,     |      ,     |    .   |
|   Total   |   ___,___  |   ___,___  |    .   |


Prebuilt
=================

- C/C++ interface code can be obtained from /src_c<br>
- Verilog code can be obtained from /src_ver<br>

Note
=================
- [TBD]

License
=======

x266 is distributed under the terms of the Private/Education ONLY License.
See COPYRIGHT for more details.

See `LICENSE.TXT` for more details.

Creator on November 2015<br>
Copyright (c) 2015-2018 Min Chen<br>
Contact: Min Chen <chenm003@{163, gmail}.com><br>
