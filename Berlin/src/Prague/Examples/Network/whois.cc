#include <Prague/IPC/sockstream.hh>
#include <iostream>

using namespace Prague;

int main(int argc, char **argv)
{
  if (argc != 2 && argc != 3)
    {
      std::cerr << "Usage: " << argv[0] << " [ -l ] user_name\n";
      return 1;
    }
  iosockinet sio(sockbuf::sock_stream);
  if (argc == 3)
    {
      if (argv[1][0] != '-' || argv[1][1] != 'l')
	{
	  std::cerr << "Usage: " << argv[0] << " [ -l ] user_name\n";
	  return 1;
	}
      // use local whois server
      sio->connect(sockinetaddr("128.143.2.20", "whois", "tcp")); 
      sio << argv[2] << "\r\n" << std::flush;
    }
  else
    {
      sio->connect(sockinetaddr("whois.internic.net", "whois", "tcp"));
      sio << argv[1] << "\r\n" << std::flush;
    }
  char buf[1024];
  while (sio.getline(buf, 1023)) std::cout << buf << std::endl;
  std::cout << std::endl;
  return 0;
}
