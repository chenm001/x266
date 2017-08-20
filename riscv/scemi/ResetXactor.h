
#ifndef RESET_XACTOR_H
#define RESET_XACTOR_H

#include <string>
#include "bsv_scemi.h"

class ResetXactor
{
public:
    ResetXactor(const std::string& hier, const std::string& name, SceMi* sceMi);
    void reset();

private:
    InportProxyT<BitT<1> > m_send;
    OutportQueueT<BitT<1> > m_recv;
};

#endif//RESET_XACTOR_H

