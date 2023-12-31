#include <Prague/Sys/Signal.hh>
#include <unistd.h>

using namespace Prague;

class Notifier : public Signal::Notifier
{
public:
  Notifier() : count(0) {}
  virtual void notify(int signum)
  {
    cerr << "catched signal '" << Signal::name(signum) << '\'' << endl;
    if (++count == 5) exit(0);
  }
private:
  int count;
};

int main(int argc, char *argv)
{
  Notifier *notifier = new Notifier();
  Signal::set(Signal::interrupt, notifier);
  Signal::set(Signal::hangup, notifier);
  Signal::set(Signal::abort, notifier);
  Signal::set(Signal::kill, notifier);
  while (1) sleep(1);
}
