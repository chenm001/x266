// Ehr.bsv
// For 6.175, Fall 2014
// 
// CM for Ehr#(2,t) ehr:
//
//                  |   ehr[0]._read    ehr[0]._write   ehr[1]._read    ehr[1]._write
// -----------------+---------------------------------------------------------------
// ehr[0]._read     |       CF              <               CF              <
// ehr[0]._write    |       >               C               <               <
// ehr[1]._read     |       CF              >               CF              <
// ehr[1]._write    |       >               >               >               C

import Vector::*;
import RWire::*;
import RevertingVirtualReg::*;

typedef Vector#(n, Reg#(t)) Ehr#(numeric type n, type t);

module mkEhr( t initVal, Ehr#(n,t) ifc ) provisos(Bits#(t, tSz));
    Reg#(t) ehrReg <- mkReg( initVal );

    // Allows read(i+1) to be in the same cycle as write(i)
    Vector#(n, RWire#(t)) wires <- replicateM( mkUnsafeRWire );

    // Additional objects to force the requested schedule
    // These will both be optimized out during FPGA synthesis
    Vector#(n, Reg#(Bool)) virtual_reg <- replicateM( mkRevertingVirtualReg(False) );
    Vector#(n, RWire#(t)) ignored_wires <- replicateM( mkUnsafeRWire );

    Ehr#(n,t) ifc_to_return;

    // These attributes are statically checked by the compiler
    (* fire_when_enabled *)         // WILL_FIRE == CAN_FIRE
    (* no_implicit_conditions *)    // CAN_FIRE == guard (True)
    rule canonicalize;
        t val = ehrReg;
        for( Integer i = 0 ; i < valueOf(n) ; i = i + 1 ) begin
            val = fromMaybe( val, wires[i].wget );
        end
        ehrReg <= val;
    endrule

    for( Integer i = 0 ; i < valueOf(n) ; i = i + 1 ) begin
        ifc_to_return[i] =
            (interface Reg;
                method Action _write(t x);
                    // Performs write
                    wires[i].wset(x);

                    // Ensures write j < write i for j < i
                    t ignore = ehrReg;
                    for( Integer j = 0 ; j < i ; j = j+1 ) begin
                        ignore = fromMaybe( ignore, wires[j].wget );
                    end
                    // Do something with ignore so the compiler doesn't optimize it out
                    ignored_wires[i].wset(ignore);

                    // Helps ensure that read j < write i for j <= i
                    virtual_reg[i] <= True;
                endmethod
                method t _read;
                    // Gets the value to return
                    // Also ensures that write j < read i for j < i
                    t val = ehrReg;
                    for( Integer j = 0 ; j < i ; j = j+1 ) begin
                        val = fromMaybe( val, wires[j].wget );
                    end

                    // Helps ensure that read i < write j for i <= j
                    for( Integer j = i ; j < valueOf(n) ; j = j+1 ) begin
                        if( virtual_reg[j] ) begin
                            // This is impossible because virtual_reg will always be False when read
                            val = unpack(0);
                        end
                    end

                    // Return the value read
                    return val;
                endmethod
            endinterface);
    end
    return ifc_to_return;
endmodule

module mkEhrU( Ehr#(n,t) ) provisos(Bits#(t, tSz));
    let m <- mkEhr( ? );
    return m;
endmodule

