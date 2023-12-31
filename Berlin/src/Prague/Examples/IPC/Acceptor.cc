#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/File.hh>
#include <Prague/Sys/GetOpt.hh>
#include <Prague/IPC/Acceptor.hh>
#include <string>
#include <unistd.h>

using namespace Prague;
bool running;

class Connection : public SocketAgent
{
public:
  Connection(sockbuf *socket) : SocketAgent(socket)
  {
    Trace trace("Connection::Connection");
  }
  ~Connection() { Trace trace("Connection::~Connection"); ::running = false;}
private:
  virtual bool process(int, iomask)
  {
    Trace trace("Connection::process");
    std::istream is(obuf());
    std::string line;
    std::getline(is, line);
    std::ostream os(ibuf());
    os << "welcome !" << std::endl;
    stop();
    return false;
  }
};

int main (int argc, char **argv)
{
  GetOpt getopt(argv[0], "an async (unix) socket client");
  getopt.add('t', "trace", GetOpt::novalue, "switch tracing on");
  getopt.parse(argc, argv);
  std::string value;
  getopt.get("trace", &value);
  if (value == "true") Tracer::logging(true);
  sockunixbuf *socket = new sockunixbuf(sockbuf::sock_stream);
  std::string name = File::tmp();
  socket->bind(name);
  Agent *acceptor = new Acceptor<Connection>(socket);
  running = true;
  acceptor->start();
  acceptor->remove_ref();
  std::cout << "server is accepting connection requests at " << name << std::endl;
  while (running) sleep(1);
}
