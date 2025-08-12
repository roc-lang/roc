#include <chrono>
#include <thread>

#include "public/client/TracyProfiler.hpp"

extern "C" void ___tracy_wait_shutdown(void) {
  tracy::GetProfiler().RequestShutdown();
  while(!tracy::GetProfiler().HasShutdownFinished()) {
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
  };
}
