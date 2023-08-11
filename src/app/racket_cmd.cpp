#include <cstdint>
#include <iostream>

#include "bind/bindings.h"

auto main(int argc, const char** argv) -> int {
  std::string opt;
  if (argc < 2) {
    std::cout << "Usage: racket_cmd config.scm [-i / -c commands]\n";
    return -1;
  }

  if (argc > 2) {
    opt = argv[2];
  }

  bind::ScriptingWorker script(true);

  std::string path = argv[1];

  script.initSync(path);
  // Interactive Scheme
  if (opt == "-i") {
    std::cout << "Repl:\n";
    script.replSync();
  } else if (opt == "-c" && argc > 3) {
    for (int i = 3; i < argc; i++) {
      bind::ScriptingWorker::scmEvalString(argv[i]);
    }
  }

  return 0;
}
