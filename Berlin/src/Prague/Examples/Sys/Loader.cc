#include <Prague/Sys/Plugin.hh>
#include <string>
#include <iostream>
#include <unistd.h>
#include "Action.hh"

using namespace Prague;

int main(int, char **)
{
  char cwd[128];
  getcwd(cwd, 128);
  std::string plugin1 = std::string(cwd) + "/Plugin1.so";
  std::string plugin2 = std::string(cwd) + "/Plugin2.so";
  for (int i = 0; i != 5; i++)
    {
      Plugin<Action> action1(plugin1);
      Plugin<Action> action2(plugin2);
      if (action1) action1->execute();
      else std::cerr << action1.error() << std::endl;
      if (action2) action2->execute();
      else std::cerr << action2.error() << std::endl;
    }
}
