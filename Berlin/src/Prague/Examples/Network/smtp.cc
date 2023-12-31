#include <Prague/Network/smtp.hh>
#include <Prague/Sys/User.hh>
#include <string>
// #include <stdio.h>
// #include <pwd.h>
#include <unistd.h>

using namespace Prague;

int main (int argc, char **argv)
{
  if (argc < 4)
    {
      cerr << "Usage: " << argv[0] << " host recipient files...\n";
      return 1;
    }
  smtp client (&cout);

  // establish connection
  client->connect(argv[1]);
  client->helo();

  // get help
  client->help ();

  // get the FROM address
  User me;
  string sender = string (me.name()) + '@' + client->localhost();

  // send the files
  for (int i = 3; i < argc; i++)
    {
      client->mail(sender.c_str());
      client->rcpt(argv [2]);
      client->data(argv [i]);
      client->rset();
    }

  // finally quit
  client->quit();

  return 0;
}
