
import GetPut::*;

import Types::*;

typedef Data MemResp;

typedef enum{Ld, St} MemOp deriving(Eq, Bits, FShow);
typedef struct{
    MemOp op;
    Addr  addr;
    Data  data;
} MemReq deriving(Eq, Bits, FShow);
