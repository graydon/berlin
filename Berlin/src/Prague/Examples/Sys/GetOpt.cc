#include <Prague/Sys/GetOpt.hh>

int main(int argc, char **argv)
{
  GetOpt getopt1(argv[0], "nothing here");
  GetOpt getopt2(argv[0], "nothing as well");
  getopt1.add('v', "version", GetOpt::novalue, "version number");
  getopt1.add('h', "help", GetOpt::novalue, "help message");
}
