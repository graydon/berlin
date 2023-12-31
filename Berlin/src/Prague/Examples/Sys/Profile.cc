#include <Prague/Sys/Profiler.hh>
#include <unistd.h>

using namespace Prague;

void foo()
{
  Profiler prf("foo");
  sleep(1);
}

void bar()
{
  Profiler prf("bar");
  foo();
}

int main(int, char **)
{
  Profiler prf("main");
  bar();
  Profiler::dump(cout);
}
