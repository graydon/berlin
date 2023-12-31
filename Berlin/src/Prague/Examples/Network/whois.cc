#include <Prague/IPC/sockstream.hh>

using namespace Prague;

int main(int argc, char **argv)
{
  if (argc != 2 && argc != 3)
    {
      cerr << "Usage: " << argv[0] << " [ -l ] user_name\n";
      return 1;
    }
  iosockinet sio(sockbuf::sock_stream);
  if (argc == 3)
    {
      if (argv[1][0] != '-' || argv[1][1] != 'l')
	{
	  cerr << "Usage: " << argv[0] << " [ -l ] user_name\n";
	  return 1;
	}
      // use local whois server
      sio->connect("128.143.2.20", "whois", "tcp"); 
      sio << argv[2] << "\r\n" << flush;
    }
  else
    {
      sio->connect("nic.ddn.mil", "whois", "tcp");
      sio << argv[1] << "\r\n" << flush;
    }
  char buf[1024];
  while (sio.getline(buf, 1023)) cout << buf << endl;
  cout << endl;
  return 0;
}
