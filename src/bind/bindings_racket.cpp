
#include <libgen.h>

#include <chrono>
#include <csignal>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <utility>

#include <string.h>
#include <unistd.h>

#include "bind/bindings.h"
#include "rkt_bind_version.h"

extern "C" {
#include "racket/chezscheme.h"
#include "racket/racketcs.h"
}

#include "racket_base.h"

static ptr lookup_var;

static const uint32_t RKT_BIND_WAIT_SLEEP = 500;


void bSetLookup(ptr lv) {
  Slock_object(lv);
  lookup_var = lv;
}

auto scmGetVar(const char* id) -> ptr {
  ptr ret = Scar(racket_apply(lookup_var, Scons(Sstring_to_symbol(id), Snil))); 
  Slock_object(ret);
  return ret;
}

static inline auto scmGetString(ptr s) -> std::string {
  constexpr size_t BUFF_SZ{ 8192 };
  char buf[BUFF_SZ];             
  uint len = Sstring_length(s);  
  if (len < BUFF_SZ) {
    int idx = 0;
    for (idx = 0; idx < len; idx++) {
      buf[idx] = Sstring_ref(s, idx);  
    }
    buf[idx] = 0;
    return { buf, len };
  }
  return "";
}

// Racket function 'debug-log', takes a Scheme string as argument
static auto bDebugLog(ptr str) -> void {
  // Log immediately in the Racket thread context
  std::cout << scmGetString(str) << "\n";
}


// Racket function 'get-rkt-bind-version', returns a new Scheme string every time it's called
static auto bGetVersionString() -> ptr {
  return Sstring(get_rkt_bind_version().c_str());
}


// The following bindings are for the PID demo

// Return the current control variable value as double
static auto bGetControlVar(ptr ctx) -> double {
  return static_cast<bind::PIDDemoInterface*>(Sunbox(ctx))->getControlVar();
}

// Return the number of tick as integer
static auto bGetNTick(ptr ctx) -> int {
  return static_cast<bind::PIDDemoInterface*>(Sunbox(ctx))->getTickCount();
}

// Set the control parameter as double
static auto bSetControlParameter(ptr ctx, double pv) -> void {
  return static_cast<bind::PIDDemoInterface*>(Sunbox(ctx))->setControlParameter(pv);
}

static auto demoRacketCallbacks() -> void {
#define FUN(nm, fn, ...) Sforeign_symbol("(demo)" nm, reinterpret_cast<void*>(&fn));
#include "bindings.inc"
#undef FUN
}

static racket_boot_arguments_t ba;
static std::string boot1_path;
static std::string boot2_path;
static std::string boot3_path;

static void getExecPath(char* path, size_t len) {
  size_t buff_len;
  if ((buff_len = readlink("/proc/self/exe", path, len - 1)) != -1) {
    path[buff_len] = '\0';
    dirname(path);
    strcat(path, "/");
  }
}

void rktSigHandler(int sig) {
  fflush(stdout);
  sleep(1);  // Not sure this sleeps needs to be a full second.
  exit(-1);
}

void bind::ScriptingWorker::init_racket() {
  constexpr size_t FILE_SZ{ 8192 };
  static char exec_file[FILE_SZ]; 
  getExecPath(exec_file, FILE_SZ);
  memset(&ba, 0, sizeof(ba));
  boot1_path = std::string(exec_file) + "racket/petite.boot";
  {
    std::ifstream ftmp(boot1_path.c_str());
    if (!ftmp.good()) {
      boot1_path = "/usr/lib/racket/petite.boot";
      boot2_path = "/usr/lib/racket/scheme.boot";
      boot3_path = "/usr/lib/racket/racket.boot";
    } else {
      boot2_path = std::string(exec_file) + "racket/scheme.boot";
      boot3_path = std::string(exec_file) + "racket/racket.boot";
    }
  }

  ba.boot1_path = boot1_path.c_str();
  ba.boot2_path = boot2_path.c_str();
  ba.boot3_path = boot3_path.c_str();
  ba.collects_dir = "/usr/share/racket/collects";
  ba.exec_file = exec_file;

  racket_boot(&ba);
  init_racket_base();
  demoRacketCallbacks();
  
  racket_namespace_require(Sstring_to_symbol("racket/base"));
  racket_namespace_require(Sstring_to_symbol("racket/load"));
  racket_namespace_require(Sstring_to_symbol("demo_common"));
  std::signal(SIGINT, rktSigHandler);
}

static auto toBytevector(const char* s) -> ptr {
  iptr len = strlen(s);
  ptr bv = Smake_bytevector(len, 0);     
  memcpy(Sbytevector_data(bv), s, len);  
  return bv;
}

auto bind::ScriptingWorker::scmCall(const char* fn) -> void* {
  return (void*)racket_eval(Scons(Sstring_to_symbol(fn), Snil)); 
}

void bind::ScriptingWorker::scmCallScriptedUpdate() {
  static ptr vref = scmGetVar("scripted-update");
  racket_apply(vref, Snil);
}

void bind::ScriptingWorker::scmEvalString(const char* cmd) {
  static ptr cs_eval = scmGetVar("cs-eval");
  ptr e = toBytevector(cmd);
  e = Scons(e, Snil);
  racket_apply(cs_eval, e);
}

void bind::ScriptingWorker::scmPrimitiveLoad(const char* path) {
  ptr e = Sstring(path);
  e = Scons(Sstring_to_symbol("load"), Scons(e, Snil));
  racket_eval(e);
}

auto bind::ScriptingWorker::scmRegisterCallback(void* p) -> bool {
  ptr e = Sbox(p);
  e = Scons(e, Snil);                                                          
  return Scar(racket_eval(Scons(Sstring_to_symbol("register-callback"), e))); 
}



bind::ScriptingWorker::ScriptingWorker(bool syncp, std::string cfgpath,
                                       void *context)
  : syncp_{syncp}
  , config_path_{cfgpath}
  , context_ptr_(context)
{
  if (!syncp) {
    worker_ = std::make_unique<std::thread>(&bind::ScriptingWorker::runner, this);
  }
}

void bind::ScriptingWorker::command() {
  command_ = true;
  sync_.store(true);
}

auto bind::ScriptingWorker::wait(size_t timeout, bool busy) -> bool {
  auto now = std::chrono::high_resolution_clock::now();
  bool expected = true;

  // Spin & wait for signal / timeout
  while (!revsync_.compare_exchange_strong(expected, false)) {
    expected = true;

    if (timeout > 0) {
      if (std::chrono::duration_cast<std::chrono::microseconds>(
              std::chrono::high_resolution_clock::now() - now)
              .count() > timeout) {
        // Script thread return timeout... Who cares?
        return false;
      }
    }
    if (!busy) {
      std::this_thread::sleep_for(std::chrono::milliseconds(RKT_BIND_WAIT_SLEEP));
    }
  }
  return_ = false;
  return true;
}

void bind::ScriptingWorker::kill() {
  if (!terminate_) {
    terminate_ = true;
    command();
    if (!syncp_) {
      worker_->join();
    }
  }
}

void bind::ScriptingWorker::signalCompletion() {
  return_ = true;
  revsync_.store(true);
}

void bind::ScriptingWorker::initSync(std::string& path) {
  if (syncp_) {
    init_racket();
    scmPrimitiveLoad(path.c_str());
    ptr tmp = Scons(Sstring_to_symbol("set_lookup"), Scons(Sstring_to_symbol("lookup-var"), Snil));
    racket_eval(tmp);
  }
}

void bind::ScriptingWorker::replSync() {
  if (syncp_) {
    racket_apply(
        Scar(racket_dynamic_require(Sstring_to_symbol("racket/base"),
                                    Sstring_to_symbol("read-eval-print-loop"))),
        Snil);
  }
}

void bind::ScriptingWorker::runner() {
  init_racket();

  // Load racket script specified by environment variable
  const char *path = nullptr;

  if (!config_path_.empty()) path = config_path_.c_str();
  else path = getenv("RACKET_BINDINGS_CONFIG");
  if (path != nullptr) {
    scmPrimitiveLoad(path);
    ptr tmp = Scons(Sstring_to_symbol("set_lookup"), Scons(Sstring_to_symbol("lookup-var"), Snil));
    racket_eval(tmp);
  }

  if (!scmRegisterCallback(reinterpret_cast<void*>(context_ptr_))) {
    exit(-1);  // failure
  }

  signalCompletion();  // for the host thread waiting for initialization completion

  for (;;) {
    {
      while (true) {
        std::this_thread::sleep_for(std::chrono::microseconds(scripting_ping_period_));
        bool expected = true;

        // Spin & wait for signal
        if (!sync_.compare_exchange_strong(expected, false)) {
          expected = true;
        } else {
          command_ = false;
          break;
        }
      }
    }
    if (terminate_) {
      return;
    }

    scmCallScriptedUpdate();
    
    signalCompletion();
  }
}

bind::ScriptingWorker::~ScriptingWorker() {
  kill();
}
