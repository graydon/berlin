#include <Prague/Sys/Signal.hh>
#include <Prague/Sys/Timer.hh>
#include <unistd.h>

using namespace Prague;

const char *text1[] = {"Heut' kommt der Hans zu mir,",
		       "Freut sich die Lies'.",
		       "Kommt er aber \"uber Oberammergau,",
		       "Oder aber \"uber Unterammergau,",
		       "Ob er aber \"uberhaupt nicht kommt,",
		       "Ist nicht gewiss."};
const char *text2[] = {"C-A-F-F-E-E,",
		       "Trink nicht so viel Kaffee.",
		       "Nicht f\"ur Kinder dieser T\"urkentrank,",
		       "Schw\"acht die Nerven, macht dich schwach und krank.",
		       "Sei doch kein Muselman,",
		       "Der das nicht lassen kann."};

class Singer : public Timer::Notifier
{
public:
  Singer(const char **s, const char *c) : song(s), color(c), line(0) {}
  virtual void notify() { std::cout << color << song[line % 6] << std::endl; line++;}
private:
  const char **song;
  const char  *color;
  short line;
};

class Exit : public Signal::Notifier
{
public:
  Exit(Timer *t1, Timer *t2) : timer1(t1), timer2(t2), count(0) {}
  virtual void notify(int)
    {
      switch (++count)
	{
	case 1: timer1->stop(); break;
	case 2: timer2->stop(); timer1->start(Time::currentTime(), 1000); break;
	case 3: timer2->start(Time::currentTime(), 1000); break;
	case 4: std::cout << "\033[0m" << std::flush; exit(0);
	};
    }
private:
  Timer *timer1, *timer2;
  int count;
};

int main (int argc, char **argv)
{
  Singer *bass = new Singer(text1, "\033[31m");
  Singer *soprano = new Singer(text2, "\033[32m");
  Timer *timer1 = new Timer(bass);
  Timer *timer2 = new Timer(soprano);
  Exit *exit = new Exit(timer1, timer2);
  Signal::set(Signal::interrupt, exit);
  timer1->start(Time::currentTime(), 1000);
  timer2->start(Time::currentTime(), 1000);
  while (true) sleep(1);
}
