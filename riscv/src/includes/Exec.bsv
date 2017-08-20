/*

Copyright (C) 2012

Arvind <arvind@csail.mit.edu>
Derek Chiou <derek@ece.utexas.edu>
Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import MemTypes::*;
import ProcTypes::*;
import Vector::*;

(* noinline *)
function Data alu(Data a, Data b, AluFunc func);
  Data res = case(func)
     Add   : (a + b);
     Sub   : (a - b);
     And   : (a & b);
     Or    : (a | b);
     Xor   : (a ^ b);
     Slt   : zeroExtend( pack( signedLT(a, b) ) );
     Sltu  : zeroExtend( pack( a < b ) );
	 // 5-bit shift width for 32-bit data
     Sll   : (a << b[4:0]);
     Srl   : (a >> b[4:0]);
     Sra   : signedShiftRight(a, b[4:0]);
  endcase;
  return res;
endfunction

(* noinline *)
function Bool aluBr(Data a, Data b, BrFunc brFunc);
  Bool brTaken = case(brFunc)
    Eq  : (a == b);
    Neq : (a != b);
    Lt  : signedLT(a, b);
    Ltu : (a < b);
    Ge  : signedGE(a, b);
    Geu : (a >= b);
    AT  : True;
    NT  : False;
  endcase;
  return brTaken;
endfunction

(* noinline *)
function Addr brAddrCalc(Addr pc, Data val, IType iType, Data imm, Bool taken);
  Addr pcPlus4 = pc + 4;
  Addr targetAddr = case (iType)
    J  : (pc + imm);
    Jr : {truncateLSB(val + imm), 1'b0};
    Br : (taken ? pc + imm : pcPlus4);
    default: pcPlus4;
  endcase;
  return targetAddr;
endfunction

(* noinline *)
function ExecInst exec(DecodedInst dInst, Data rVal1, Data rVal2, Addr pc, Addr ppc, Data csrVal);
  ExecInst eInst = ?;

  // do ALU operation: use imm instead of rs2 if imm is valid (consider LW and SW)
  Data aluVal2 = isValid(dInst.imm) ? fromMaybe(?, dInst.imm) : rVal2;
  let aluRes = alu(rVal1, aluVal2, dInst.aluFunc);
  
  // set eInst
  eInst.iType = dInst.iType;
  eInst.dst = dInst.dst;
  eInst.csr = dInst.csr;
  
  eInst.data = dInst.iType == Csrr ?
                 csrVal :
			   dInst.iType == Csrw ?
			     rVal1 :
               dInst.iType==St ?
                 rVal2 :
               (dInst.iType==J || dInst.iType == Jr) ?
                 (pc+4) :
               dInst.iType==Auipc ?
                 (pc + fromMaybe(?, dInst.imm)) :
                 aluRes;

  let brTaken = aluBr(rVal1, rVal2, dInst.brFunc);
  let brAddr = brAddrCalc(pc, rVal1, dInst.iType, fromMaybe(?, dInst.imm), brTaken);

  eInst.addr = (dInst.iType == Ld || dInst.iType == St) ? aluRes : brAddr;
  eInst.mispredict = brAddr != ppc;
  eInst.brTaken = brTaken;
  
  return eInst;
endfunction

