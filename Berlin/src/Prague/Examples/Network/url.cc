#include <Prague/Network/url.hh>
#include <iostream>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc != 2)
    {
      std::cerr << "Usage : " << argv[0] << " url\n";
      return 1;
    }
  url u(argv[1]);
}
    
