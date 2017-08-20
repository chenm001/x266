import Ehr::*;
import Vector::*;
import GetPut::*;

//////////////////
// SFifo interface 

interface SFifo#(numeric type n, type dt, type st);
    method Bool search(st x);
    method Bool notFull;
    method Action enq(dt x);
    method Bool notEmpty;
    method Action deq;
    method dt first;
    method Action clear;
endinterface

//////////////////
// Pipeline SFIFO

// deq < search < enq < clear
module mkPipelineSFifo( function Bool isFound(dt x, st y), SFifo#(n, dt, st) ifc ) provisos (Bits#(dt,dtSz));
    // n is size of the FIFO
    // dt is data type
    // st is search type
    Vector#(n, Reg#(dt))    data     <- replicateM(mkRegU());
    Ehr#(2, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(2, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           empty    <- mkEhr(True);
    Ehr#(3, Bool)           full     <- mkEhr(False);
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool search(st x);
        // look between deqP[1] and enqP[0]
        Bool ret = False;
        Bool enqP_lt_deqP = enqP[0] < deqP[1];
        for( Integer i = 0 ; i < valueOf(n) ; i = i+1 ) begin
            Bool lt_enqP = fromInteger(i) < enqP[0];
            Bool gte_deqP = fromInteger(i) >= deqP[1];
            Bool is_match = isFound(data[i], x);
            if( full[1] || (enqP_lt_deqP && lt_enqP) || (enqP_lt_deqP && gte_deqP) || (lt_enqP && gte_deqP) ) begin
                // index i is valid
                if( is_match ) begin
                    ret = True;
                end
            end
        end
        return ret;
    endmethod

    method Bool notFull = !full[1];

    method Action enq(dt x) if( !full[1] );
        data[enqP[0]] <= x;
        empty[1] <= False;
        enqP[0] <= (enqP[0] == max_index) ? 0 : enqP[0] + 1;
        if( enqP[1] == deqP[1] ) begin
            full[1] <= True;
        end
    endmethod

    method Bool notEmpty = !empty[0];

    method Action deq if( !empty[0] );
        // Tell later stages a dequeue was requested
        full[0] <= False;
        deqP[0] <= (deqP[0] == max_index) ? 0 : deqP[0] + 1;
        if( deqP[1] == enqP[0] ) begin
            empty[0] <= True;
        end
    endmethod

    method dt first if( !empty[0] );
        return data[deqP[0]];
    endmethod

    method Action clear;
        enqP[1] <= 0;
        deqP[1] <= 0;
        empty[2] <= True;
        full[2] <= False;
    endmethod
endmodule

////////////////
// Bypass SFIFO

// search < enq < deq < clear
module mkBypassSFifo( function Bool isFound(dt x, st y), SFifo#(n, dt, st) ifc ) provisos (Bits#(dt,dtSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Ehr#(2,dt))  data     <- replicateM(mkEhr(?));
    Ehr#(2, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(2, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           empty    <- mkEhr(True);
    Ehr#(3, Bool)           full     <- mkEhr(False);
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool search(st x);
        // look between deqP[0] and enqP[0]
        Bool ret = False;
        Bool enqP_lt_deqP = enqP[0] < deqP[0];
        for( Integer i = 0 ; i < valueOf(n) ; i = i+1 ) begin
            Bool lt_enqP = fromInteger(i) < enqP[0];
            Bool gte_deqP = fromInteger(i) >= deqP[0];
            Bool is_match = isFound(data[i][0], x);
            if( full[0] || (enqP_lt_deqP && lt_enqP) || (enqP_lt_deqP && gte_deqP) || (lt_enqP && gte_deqP) ) begin
                // index i is valid
                if( is_match ) begin
                    ret = True;
                end
            end
        end
        return ret;
    endmethod

    method Bool notFull = !full[0];

    method Action enq(dt x) if( !full[0] );
        data[enqP[0]][0] <= x;
        empty[0] <= False;
        enqP[0] <= (enqP[0] == max_index) ? 0 : enqP[0] + 1;
        if( enqP[1] == deqP[0] ) begin
            full[0] <= True;
        end
    endmethod

    method Bool notEmpty = !empty[1];

    method Action deq if( !empty[1] );
        // Tell later stages a dequeue was requested
        full[1] <= False;
        deqP[0] <= (deqP[0] == max_index) ? 0 : deqP[0] + 1;
        if( deqP[1] == enqP[1] ) begin
            empty[1] <= True;
        end
    endmethod

    method dt first if( !empty[1] );
        return data[deqP[0]][1];
    endmethod

    method Action clear;
        enqP[1] <= 0;
        deqP[1] <= 0;
        empty[2] <= True;
        full[2] <= False;
    endmethod
endmodule

// CF SFifo
// {search, enq, deq} < clear
module mkCFSFifo( function Bool isFound(dt x, st y), SFifo#(n, dt, st) ifc ) provisos (Bits#(dt,dtSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(dt)) data     <- replicateM(mkRegU);
    Reg#(Bit#(TLog#(n))) enqP     <- mkReg(0);
    Reg#(Bit#(TLog#(n))) deqP     <- mkReg(0);
    Reg#(Bool)           empty    <- mkReg(True);
    Reg#(Bool)           full     <- mkReg(False);
    Bit#(TLog#(n))       max_index = fromInteger(valueOf(n)-1);

	Ehr#(3, Maybe#(dt)) enqEn <- mkEhr(Invalid);
	Ehr#(3, Bool) deqEn <- mkEhr(False);
	Ehr#(2, Bool) clearEn <- mkEhr(False);

	function Bit#(TLog#(n)) nextPtr(Bit#(TLog#(n)) curPtr);
		return curPtr == max_index ? 0 : curPtr + 1;
	endfunction

	(* fire_when_enabled *)
	(* no_implicit_conditions *)
	rule cononicalize;
		if(clearEn[1]) begin
			enqP <= 0;
			deqP <= 0;
			full <= False;
			empty <= True;
		end
		else begin
			let enqP_nxt = enqP;
			let deqP_nxt = deqP;
			// change ptr
			if(enqEn[2] matches tagged Valid .x) begin
				data[enqP] <= x;
				enqP_nxt = nextPtr(enqP);
			end
			if(deqEn[2]) begin
				deqP_nxt = nextPtr(deqP);
			end
			enqP <= enqP_nxt;
			deqP <= deqP_nxt;
			// change full, empty
			Bool isEnq = isValid(enqEn[2]);
			Bool isDeq = deqEn[2];
			Bool nextPtrEq = deqP_nxt == enqP_nxt;
			if(isEnq && !isDeq) begin
				empty <= False;
				full <= nextPtrEq;
			end
			else if(!isEnq && isDeq) begin
				full <= False;
				empty <= nextPtrEq;
			end
		end
		// clear enables
		clearEn[1] <= False;
		enqEn[2] <= Invalid;
		deqEn[2] <= False;
	endrule

    method Bool search(st x);
        // look between deqP and enqP
        Bool ret = False;
        Bool enqP_lt_deqP = enqP < deqP;
        for( Integer i = 0 ; i < valueOf(n) ; i = i+1 ) begin
            Bool lt_enqP = fromInteger(i) < enqP;
            Bool gte_deqP = fromInteger(i) >= deqP;
            Bool is_match = isFound(data[i], x);
            if( full || (enqP_lt_deqP && lt_enqP) || (enqP_lt_deqP && gte_deqP) || (lt_enqP && gte_deqP) ) begin
                // index i is valid
                if( is_match ) begin
                    ret = True;
                end
            end
        end
        return ret;
    endmethod

    method Bool notFull = !full;

    method Action enq(dt x) if( !full );
		enqEn[0] <= Valid (x);
    endmethod

    method Bool notEmpty = !empty;

    method Action deq if( !empty );
		deqEn[0] <= True;
    endmethod

    method dt first if( !empty );
        return data[deqP];
    endmethod

    method Action clear;
		clearEn[0] <= True;
		enqEn[1] <= Invalid;
		deqEn[1] <= False;
    endmethod
endmodule

