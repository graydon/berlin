#include <Prague/IPC/TTYAgent.hh>
#include <Prague/Sys/Tracer.hh>
#include <string>

using namespace Prague;

Coprocess *agent;

class Output : public Coprocess::IONotifier
{
public:
  virtual bool notify(Agent::iomask mask)
  {
    if (mask != Agent::outready) return false;
    std::istream is(agent->obuf());
    std::string line;
    while (is)
      {
	std::getline(is, line);
	std::cout << line;
	if (is) std::cout << std::endl;
	else std::cout << std::flush;
      }
    return true;
  }
};

class ConnectionClosed : public Coprocess::EOFNotifier
{
public:
  virtual void notify(Agent::iomask mask)
  {
    if (mask == Agent::outready)
      std::cout << "output : connection closed";
  }
};

void *start(void *)
{
  while (std::cin && agent->ibuf())
    {
      std::ostream os(agent->ibuf());
      std::string line;
      std::getline(std::cin, line);
      os << line << std::endl;
      Thread::delay(500);
    }
  return 0;
}


int main (int argc, char **argv)
{
  Tracer::logging(true);
  Output *out = new Output;
  ConnectionClosed *eof = new ConnectionClosed;
  agent = new TTYAgent("sh", out, eof);
  Thread thread(start, 0);
  agent->start();
  thread.start();
  thread.join(0);
  if (!agent->ibuf())
    {
      if (agent->state() == Coprocess::exited)
	std::cerr << "process exited with value " << agent->value() << std::endl;
      else if (agent->state() == Coprocess::signaled)
	std::cerr << "process killed with signal " << agent->value() << std::endl;
    }
  delete agent;
  delete eof;
  delete out;
}
