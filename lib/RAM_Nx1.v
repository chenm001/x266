// Copyright (c) 2016 Min Chen
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

//
// Asymmetric port RAM

module RAM_Nx1(clkA, clkB, weA, reB, addrA, addrB, diA, doB);

  parameter WIDTHA      = 72;
  parameter SIZEA       = 512;
  parameter ADDRWIDTHA  = 9;
  parameter WIDTHB      = 18;
  parameter SIZEB       = 2048;
  parameter ADDRWIDTHB  = 11;

  input                         clkA;
  input                         clkB;
  input                         weA;
  input                         reB;
  input       [ADDRWIDTHA-1:0]  addrA;
  input       [ADDRWIDTHB-1:0]  addrB;
  input       [WIDTHA-1:0]      diA;
  output reg  [WIDTHB-1:0]      doB;

  reg [WIDTHA-1:0] mux;
  reg [WIDTHA-1:0] RAM [SIZEA-1:0] /* synthesis syn_ramstyle="no_rw_check" */ ;

  always @(posedge clkA)
  begin
    if(weA)
      RAM[addrA] <= diA;
  end

  always @(posedge clkB)
  begin
    mux = RAM[addrB[ADDRWIDTHB-1:1]];
    if(reB)
    begin
      if (addrB[0])
        doB <= mux[WIDTHA-1:WIDTHB];
      else
        doB <= mux[WIDTHB-1:0];
    end
  end
endmodule
