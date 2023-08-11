#ifndef RKT_BINDINGS_H
#define RKT_BINDINGS_H

#include <array>
#include <thread>
#include <atomic>

namespace bind {

class PIDDemoInterface {
public:
  virtual auto getTickCount() -> int {return 0;};
  virtual auto getControlVar() -> double {return 0.0;};
  virtual auto setControlParameter(double) -> void {};
};
  
class ScriptingWorker {
private:
  std::string config_path_;
  std::atomic<bool> sync_{ false };
  std::atomic<bool> revsync_{ false };
  static constexpr size_t scripting_ping_period_{ 25 };  // microseconds

  bool command_{ false };
  bool return_{ false };
  bool terminate_{ false };
  bool initp_{ false };

  void * context_ptr_;

  std::unique_ptr<std::thread> worker_;

  void runner();
  static void init_racket();
  static void* scmCall(const char* fn);
  static void scmCallScriptedUpdate();
  static void scmPrimitiveLoad(const char* path);
  static bool scmRegisterCallback(void* ptr);
  void signalCompletion();

public:
  bool syncp_;
  ScriptingWorker(bool sync = false, std::string cfgpath = "", void * context = nullptr);
  ~ScriptingWorker();
  void command();                               // To be called from the wrapper thread
  bool wait(size_t timeout, bool busy = true);  // to be called from the wrapper thread
  void kill();                                  // kill the worker thread
  static void scmEvalString(const char* cmd);   // should be called from the worker thread

  void initSync(std::string& path);
  void replSync();
};
}  // namespace bind

#endif  // RKT_BINDINGS_H
