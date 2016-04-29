// Copyright (c) 2016 Min Chen
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
  reg [WIDTHA-1:0] RAM [SIZEA-1:0];

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
