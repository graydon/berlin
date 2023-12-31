#include <Prague/Sys/File.hh>
#include <Prague/IPC/mmapbuf.hh>
#include <unistd.h>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc != 2)
    {
      std::cerr << "Usage : " << argv[0] << " <filename>\n";
      return -1;
    }
  std::string file = argv[1];
  std::streambuf *mbuf = new mmapbuf(file, -1, std::ios::in);
  std::istream is(mbuf);
  std::string buf;
  std::getline(is, buf);
  std::cout << "read : " << buf << std::endl;
  delete mbuf;
}
