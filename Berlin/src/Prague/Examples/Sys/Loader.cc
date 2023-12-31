#include "Prague/Sys/Plugin.hh"
#include "Action.hh"
#include <string>
#include <iostream>
#include <unistd.h>

using namespace Prague;

int main(int, char **)
{
  char cwd[128];
  getcwd(cwd, 128);
  string plugin = string(cwd) + "/Plugin.so";
  Plugin<Action> action(plugin);
  if (action) action->execute();
  else cerr << action.error() << endl;
}
