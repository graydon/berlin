#include <Prague/Sys/Plugin.hh>
#include <iostream>
#include "Action.hh"

class Greeting1 : public Action
{
  virtual void execute() { std::cout << "Hello World !" << std::endl;}
};

dload(Greeting1)
