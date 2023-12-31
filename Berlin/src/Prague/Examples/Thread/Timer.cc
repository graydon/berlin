/*$Id: Timer.cc,v 1.1 1999/09/30 17:23:34 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

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
  virtual void notify() { cout << color << song[line % 6] << endl; line++;}
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
	case 4: cout << "\033[0m" << flush; exit(0);
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
