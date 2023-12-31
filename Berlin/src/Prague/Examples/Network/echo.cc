#include <Prague/Network/echo.hh>
#include <cstdlib>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc < 3)
    {
      cerr << "Usage : " << argv[0] << " remote-host portno string...\n";
      return 1;
    }
  int portno = atoi (argv[2]);
  echo e(protocol::tcp);
  cout << e->rfc_name() << ' ' << e->rfc_doc() << endl;
  try
    {
      if (portno == 0) e->connect(argv[1]);
      else e->connect(argv[1], portno);
    }
  catch (sockerr &se)
    {
      cerr << "Error : " << se.errstr() << endl;
      exit(0);
    }
  for (int i = 3; i < argc; i++)
    {
      char reply[256];
      cout << "sending: " << argv[i] << endl;
      e << argv[i] << endl;
      e.getline(reply, 255);
      cout << "got back: " << reply << endl;
    }
  return 0;
}
