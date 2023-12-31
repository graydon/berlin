#include <Prague/Sys/Plugin.hh>
#include <iostream>
#include "Action.hh"

class Greeting2 : public Action
{
  virtual void execute() { std::cout << "Hi There !" << std::endl;}
};

dload(Greeting2)
