#include <Prague/Filter/gzstream.hh>

using namespace Prague;

int main()
{
  // Construct a stream object with this filebuffer.  Anything sent
  // to this stream will go to standard out.
  gzofstream os(1, std::ios::out);

  // This text is getting compressed and sent to stdout.
  // To prove this, run 'test | zcat'.
  os << "Hello, world" << std::endl;

  os << set_compressionlevel(Z_NO_COMPRESSION);
  os << "hello, hello, hi, ho!" << std::endl;

  set_compressionlevel(os, Z_DEFAULT_COMPRESSION)
    << "I'm compressing again" << std::endl;

  os.close();

  return 0;
}
