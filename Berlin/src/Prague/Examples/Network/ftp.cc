#include <Prague/Network/ftp.hh>
#include <Prague/Sys/Tracer.hh>
#include <iostream>
#include <unistd.h>

using namespace Prague;

int main (int argc, char **argv)
{
  Tracer::logging(true);
  if (argc != 4)
    {
      std::cerr << "Usage : " << argv[0] << " hostname user filename\n";
      return 1;
    }
  ftp f(&std::cout);

  f->connect(argv[1]);
  f->get_response();
  // set access 
  f->user(argv[2]);
  f->passwd(getpass("passwd: "));
  // get help
  f->help();
  // set representation type to image
  f->rep_type(ftp::rt_image);
  // list the home directory
  f->list();
  // quit
  f->quit();
}
    
