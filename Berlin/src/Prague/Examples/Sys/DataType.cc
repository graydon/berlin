#include <Prague/Sys/DataTypeManager.hh>
#include <iostream>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc != 3)
    {
      std::cerr << "Usage : " << argv[0] << " <database> <file>" << std::endl;
      return -1;
    }
  DataTypeManager types(argv[1]);
  std::cout << argv[2] << " is of type " << types.match(argv[2]) << std::endl;
}
