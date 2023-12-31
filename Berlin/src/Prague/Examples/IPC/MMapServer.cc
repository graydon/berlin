#include <Prague/Sys/File.hh>
#include <Prague/Sys/Process.hh>
#include <Prague/Sys/User.hh>
#include <Prague/IPC/mmapbuf.hh>
#include <unistd.h>

using namespace Prague;

int main (int argc, char **argv)
{
  File file = File::tmp();
  std::string name = file.long_name();
  std::streambuf *mbuf = new mmapbuf(name, 1024*1024, std::ios::out);
  std::ostream os(mbuf);
  os << "hi there ! from process " << Process::id() << '\n';
  std::cout << "tmp file : " << name << std::endl;
  sleep(10);
  delete mbuf;
  file.rm();
}
