#include <Prague/Network/echo.hh>
#include <cstdlib>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc != 2)
    {
      cerr << "Usage: " << argv[0] << " portno\n";
      return 1;
    }
  echo server(protocol::tcp);
  server->serve_clients(atoi(argv[1]));
  return 2;
}
