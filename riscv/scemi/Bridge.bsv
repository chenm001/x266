// Copyright Bluespec Inc. 2011-2012

`ifdef SCEMI_PCIE_VIRTEX5
  `ifdef BOARD_ML507
    `ifdef DDR2
      `include "Bridge_VIRTEX5_ML50X_DDR2.bsv"
    `else
      `include "Bridge_VIRTEX5_ML50X.bsv"
     `endif
  `endif
  `ifdef BOARD_XUPV5
    `ifdef DDR2
      `include "Bridge_VIRTEX5_ML50X_DDR2.bsv"
    `else
      `include "Bridge_VIRTEX5_ML50X.bsv"
    `endif
   `endif
`endif

`ifdef SCEMI_PCIE_VIRTEX6
  `ifdef BOARD_ML605
    `ifdef DDR3
      `include "Bridge_VIRTEX6_ML605_DDR3.bsv"
    `else
      `include "Bridge_VIRTEX6_ML605.bsv"
     `endif
  `endif
`endif

`ifdef SCEMI_PCIE_KINTEX7
  `ifdef BOARD_KC705
    `ifdef DDR3
      `include "Bridge_KINTEX7_KC705_DDR3.bsv"
    `else
      `include "Bridge_KINTEX7_KC705.bsv"
    `endif
  `endif
`endif

`ifdef SCEMI_PCIE_DINI
  `ifdef BOARD_7002
    `ifdef DDR2
      `include "Bridge_DINI_7002_DDR2.bsv"
    `else
      `ifdef SRAM
        `include "Bridge_DINI_7002_SRAM.bsv"
      `else
	`include "Bridge_DINI_7002.bsv"
      `endif
    `endif
  `endif
  `ifdef BOARD_7006
    `ifdef DDR2
      `include "Bridge_DINI_7006_DDR2.bsv"
    `else
      `ifdef SRAM
        `include "Bridge_DINI_7006_SRAM.bsv"
      `else
	`include "Bridge_DINI_7006.bsv"
      `endif
    `endif
  `endif
  `ifdef BOARD_7406
    `ifdef DDR2
      `include "Bridge_DINI_7406_DDR2.bsv"
    `else
      `ifdef SRAM
        `include "Bridge_DINI_7406_SRAM.bsv"
      `else
	`include "Bridge_DINI_7406.bsv"
      `endif
    `endif
  `endif
`endif

`ifdef SCEMI_TCP
  `include "Bridge_TCP.bsv"
`endif

`ifdef SCEMI_SCEMI
`define SCEMI_LT SCEMI
`include "Bridge_SCEMI.bsv"
`endif

`ifdef SCEMI_EVE
`define SCEMI_LT EVE
`include "Bridge_SCEMI.bsv"
`endif
