#include "Prague/Sys/Plugin.hh"
#include "Action.hh"

class Greeting : public Action
{
  virtual void execute() { cout << "Hello World !" << endl;}
};

dload(Greeting)
