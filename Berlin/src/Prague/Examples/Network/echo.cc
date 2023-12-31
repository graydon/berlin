#include <Prague/Network/echo.hh>
#include <cstdlib>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc < 3)
    {
      std::cerr << "Usage : " << argv[0] << " remote-host portno string...\n";
      return 1;
    }
  int portno = atoi (argv[2]);
  echo e(protocol::tcp);
  std::cout << e->rfc_name() << ' ' << e->rfc_doc() << std::endl;
  try
    {
      if (portno == 0) e->connect(argv[1]);
      else e->connect(argv[1], portno);
    }
  catch (sockerr &se)
    {
      std::cerr << "Error : " << se.errstr() << std::endl;
      return 0;
    }
  for (int i = 3; i < argc; i++)
    {
      char reply[256];
      std::cout << "sending: " << argv[i] << std::endl;
      e << argv[i] << std::endl;
      e.getline(reply, 255);
      std::cout << "got back: " << reply << std::endl;
    }
  return 0;
}
