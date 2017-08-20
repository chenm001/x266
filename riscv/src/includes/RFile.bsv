/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


// Correct use of the register file implies that the same index can't be used for simultaneous read and write from different rules. If different indices are used reads and writes are conflict free. If the reads and writes are in the same rule, write updates the file at the end of the rule.
// We have imitated this conflict free behavior using config regs.
// If we had used ordinary registers, then read<write
// In many designs where we needed Bypass register file, the bypassing was implemented outside the register file, explicitly.


import Types::*;
import ProcTypes::*;
import Vector::*;
import Ehr::*;
import ConfigReg::*;

interface RFile;
    method Action wr( RIndx rindx, Data data );
    method Data rd1( RIndx rindx );
    method Data rd2( RIndx rindx );
endinterface

(* synthesize *)
module mkRFile( RFile );
    Vector#(32, Reg#(Data)) rfile <- replicateM(mkReg(0));

    function Data read(RIndx rindx);
        return rfile[rindx];
    endfunction

    method Action wr( RIndx rindx, Data data );
        if(rindx!=0) begin
            rfile[rindx] <= data;
        end
    endmethod

    method Data rd1( RIndx rindx ) = read(rindx);
    method Data rd2( RIndx rindx ) = read(rindx);
endmodule

(* synthesize *)
module mkBypassRFile( RFile );
    Vector#(32, Ehr#(2,Data)) rfile <- replicateM(mkEhr(0));

    function Data read(RIndx rindx);
        return rfile[rindx][1];
    endfunction

    method Action wr( RIndx rindx, Data data );
        if(rindx!=0) begin
            rfile[rindx][0] <= data;
        end
    endmethod

    method Data rd1( RIndx rindx ) = read(rindx);
    method Data rd2( RIndx rindx ) = read(rindx);
endmodule
