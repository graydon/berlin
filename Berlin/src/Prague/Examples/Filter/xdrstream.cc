#include <Prague/Filter/xdrstream.hh>
#include <string>
#include <iostream>

using namespace Prague;

int main(int argc, char **argv)
{
  const char *file = "xdr.dat";
  if (argc != 2 || (string("-r") != argv[1] && string("-w") != argv[1]))
    {
      cout << "Usage: " << argv[0] << " -w|-r" << endl;
      exit(-1);
    };
  if (string("-r") == argv[1])
    {
      if (access(file, R_OK))
	{
	  cerr << "please use " << argv[0] << " -w first to create the data" << endl;
	  exit(-1);
	}
      int    i = 0;
      double d = 0.0;
      char   c = '\0';
      char   buffer[256];
      string s;
      ifxdrstream is(file);
      is.width(sizeof(buffer));
      is >> i >> d >> buffer >> s >> c;
      cout << i << ' ' << d << ' ' << buffer << ' ' << s << ' ' << c << endl;
    }
  else if (string("-w") == argv[1])
    {
      ofxdrstream os(file);
      os << 42 << 3.1415 << "hello," << string("world") << '!';
      if (!os) cout << "error writing binary data" << endl;
    }
  return 0;
};
