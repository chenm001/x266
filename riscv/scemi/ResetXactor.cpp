
#include "ResetXactor.h"

ResetXactor::ResetXactor(const std::string& hier, const std::string& name, SceMi* sceMi)
    : m_send(hier, name + "_dut_softrst_req_inport", sceMi),
      m_recv(hier, name + "_dut_softrst_resp_outport", sceMi)
{}

void ResetXactor::reset()
{
    m_send.sendMessage(BitT<1>(0));
    m_recv.getMessage();
}

