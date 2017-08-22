
#include <iostream>
#include <unistd.h>
#include <cmath>
#include <cstdio>
#include <cstdlib>

#include "bsv_scemi.h"
#include "SceMiHeaders.h"
#include "ResetXactor.h"


int main(int argc, char* argv[])
{
    int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );
    SceMiParameters params("scemi.params");
    SceMi *sceMi = SceMi::Init(sceMiVersion, &params);

    // Initialize the SceMi ports
    OutportQueueT<ToHost> tohost("", "scemi_tohost_get_outport", sceMi);
    InportProxyT<FromHost> fromhost("", "scemi_fromhost_put_inport", sceMi);
    ResetXactor reset("", "scemi", sceMi);
    ShutdownXactor shutdown("", "scemi_shutdown", sceMi);

    // Service SceMi requests
    SceMiServiceThread *scemi_service_thread = new SceMiServiceThread(sceMi);

	{
        // Reset the dut.
        reset.reset();

        // Start the core: start PC = 0x200
        fromhost.sendMessage(0x200);

        // Handle tohost requests.
		uint32_t print_int = 0; // integer to print
        while (true) {
            ToHost msg = tohost.getMessage();
			CpuToHostType::E_CpuToHostType type = msg.m_c2hType.m_val;
            uint16_t data = msg.m_data;

			if(type == CpuToHostType::e_ExitCode) {
				if(data == 0) {
					fprintf(stderr, "PASSED\n");
				} else {
					fprintf(stderr, "FAILED: exit code = %d\n", data);
				}
				break;
			} else if(type == CpuToHostType::e_PrintChar) {
				fprintf(stderr, "%c", (char)data);
			} else if(type == CpuToHostType::e_PrintIntLow) {
				print_int = uint32_t(data);
			} else if(type == CpuToHostType::e_PrintIntHigh) {
				print_int |= uint32_t(data) << 16;
				fprintf(stderr, "%d", print_int);
			} else {
			    fprintf(stderr, "\nUnknown type %d\n", type);
			}
        }
    }

    shutdown.blocking_send_finish();
    scemi_service_thread->stop();
    scemi_service_thread->join();
    SceMi::Shutdown(sceMi);

    return 0;
}

