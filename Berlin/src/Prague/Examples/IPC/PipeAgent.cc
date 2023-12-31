#include <Prague/Sys/Tracer.hh>
#include <Prague/IPC/PipeAgent.hh>
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
    std::getline(is, line);
    std::cout << "output : '" <<  line << '\'' << std::endl;
    return true;
  }
};

class ConnectionClosed : public Coprocess::EOFNotifier
{
public:
  virtual void notify(Agent::iomask mask)
  {
    if (mask == Agent::out)
      std::cout << "output : connection closed" << std::endl;
  }
};

int main (int argc, char **argv)
{
  if (argc == 2 && std::string("--trace") == argv[1]) Tracer::logging(true);
  Output *out = new Output;
  ConnectionClosed *eof = new ConnectionClosed;
  agent = new PipeAgent("./Echo", out, eof);
  agent->start();
  while (std::cin && agent->ibuf())
    {
      std::ostream os(agent->ibuf());
      std::string line;
      std::cout << "input :";
      do getline(std::cin, line);
      while (!std::cin && errno == EINTR);
      os << line << std::endl;
      Thread::delay(500);
    }
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
