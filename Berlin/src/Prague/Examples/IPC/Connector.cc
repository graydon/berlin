#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/File.hh>
#include <Prague/Sys/GetOpt.hh>
#include <Prague/IPC/Connector.hh>
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
    std::ostream os(ibuf());
    os << "hi there" << std::endl;
  }
  ~Connection() { Trace trace("Connection::~Connection"); ::running = false;}
private:
  virtual bool process(int, iomask)
  {
    Trace trace("Connection::process");
    std::istream is(obuf());
    std::string line;
    std::getline(is, line);
    std::cout << "server replied: " << line << std::endl;
    stop();
    return false;
  }
};

int main (int argc, char **argv)
{
  GetOpt getopt(argv[0], "an async (unix) socket client");
  getopt.add('h', "help", GetOpt::novalue, "help message");
  getopt.add('t', "trace", GetOpt::novalue, "switch tracing on");
  getopt.add('a', "address", GetOpt::mandatory, "peer address (path)");
  getopt.parse(argc, argv);
  std::string value;
  getopt.get("help", &value);
  if (value == "true")
    {
      getopt.usage();
      return 0;
    }
  value = "";
  getopt.get("trace", &value);
  if (value == "true") Tracer::logging(true);
  value = "";
  getopt.get("address", &value);
  if (value == "")
    {
      getopt.usage();
      return 1;
    }
  sockunixbuf *socket = new sockunixbuf(sockbuf::sock_stream);
  Agent *connector = new Connector<Connection, sockunixbuf>(socket, sockunixaddr(value));
  running = true;
  try
    {
      connector->start();
    }
  catch (const sockerr &e)
    {
      std::cerr << "error starting the Connector: " << e.errstr() << std::endl;
      running = false;
    }
  connector->remove_ref();
  while (running) sleep(1);
}
