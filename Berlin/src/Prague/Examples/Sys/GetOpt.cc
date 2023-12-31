#include <Prague/Sys/GetOpt.hh>
#include <iostream>
#include <string>

using namespace Prague;

int main(int argc, char **argv)
{
  /*
   * initialize some options
   */
  GetOpt getopt1(argv[0], "nothing here");
  GetOpt getopt2(argv[0], "nothing as well");
  getopt1.add('v', "version", GetOpt::novalue, "version number");
  getopt1.add('h', "help", GetOpt::novalue, "help message");
  getopt1.add('d', "drawing", GetOpt::mandatory, "the DrawingKit to choose");
  getopt1.parse(argc - 1, argv + 1);
  /*
   * now test for some options
   */
  {
    std::string version;
    getopt1.get("version", &version);
    if (version == "true") std::cout << "version is " << "..." << std::endl;
  }
  {
    std::string help;
    getopt1.get("help", &help);
    if (help == "true") getopt1.usage();
  }
  {
    std::string drawing;
    getopt1.get("drawing", &drawing);
    if (!drawing.empty()) std::cout << "drawing: " << drawing << std::endl;
  }
}
