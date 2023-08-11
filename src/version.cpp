#include <string>

#define getstr(s) #s
#define getstrstr(s) getstr(s)
static std::string version_hash = getstrstr(GIT_HASH);
static std::string git_date = GIT_DATE;
static std::string git_commit_msg = GIT_COMMIT_MSG;
static std::string git_version = "V" + version_hash + ": " + git_date + " [" + git_commit_msg + "]";

auto get_rkt_bind_version() -> std::string & { return git_version; }

