#include <chrono>
#include <cstdint>
#include <iostream>
#include <ostream>
#include <thread>

#include "bind/bindings.h"


inline static void rt_busy_wait(size_t duration) {
  auto start = std::chrono::steady_clock::now();
  for (;;) {
    auto end = std::chrono::steady_clock::now();
    auto delta = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    if (delta >= duration) {
      return;
    }
  }
}

class EventLoop : bind::PIDDemoInterface {
public:
  bind::ScriptingWorker sw_;
  EventLoop(std::string path)
    : sw_(false, path, this)
  {
    sw_.wait(0, false); // wait for initialization to complete.
    std::cout << "Initialised\n";
    std::cout.flush();
  }


  void exec() {
    // We'll pretent this is a real-time thread doing some Very Important Control.
    // Scripting thread is expected to be able to complete its work while we wait,
    // but if it miss a tick or two, it's sort of understandable and borderline forgivable.
    //
    // In an actual real-time application we'd pin the current thread to a specific CPU core,
    // and would have configured Linux to isolate this core, enable NO_HZ=full on it, etc.
    //
    // Another thing to consider here is precision of std::chrono::*
    // If your target platform is something like Zynq UltraScale+, for example, a timer functionality
    // can be offloaded to an FPGA fabric and accessed via direct memory requests for more stable
    // time reading.
    //
    for(;;) {
      auto tick_start = std::chrono::high_resolution_clock::now();
      auto tick_next  = tick_start + std::chrono::microseconds(2000);
      sw_.command();
      sw_.wait(100, true);
      // that's all we can wait, let's pretend we need the remaining 1900us
      // to do our Very Important control calculation

      pretendedBusyWork();

      tick_++;
      auto tick_remaining = std::chrono::high_resolution_clock::now();
      if (tick_remaining < tick_next) {
        // How many microseconds to wait spinning
        size_t delta = std::chrono::duration_cast<std::chrono::microseconds>(tick_next - tick_remaining).count();
        rt_busy_wait(delta);
      }
    }
  }

  void pretendedBusyWork()
  {
    // We'll simulate the pretend heating process here.
    temp_ += heat_ * heat_rate_;
    temp_ -= temp_ * cool_rate_;
    // The following bit is for demonstration only, you should not do any non-mmapped I/O in
    // a proper real-time thread:
    if (!(tick_ % 100)) {
      std::cout << "Temp=" << temp_ << "  heat= " << heat_ << "\n";
      std::cout.flush();
    }
  }


public:
  double getControlVar() override {
    return temp_;
  }

  void setControlParameter(double pv) override {
    heat_ = pv;
  }

  int getTickCount() override {
    return tick_;
  }


private:
  int tick_ { 0 };
  double temp_ { 0.0 };
  double cool_rate_ { 0.001 };
  double heat_rate_ { 0.01 };
  double heat_ { 0.0 };
};

auto main(int argc, const char** argv) -> int {
  EventLoop loop("demo_config.scm");

  loop.exec();

  return 0;
}
