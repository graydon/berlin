/*$Id: Signal.hh,v 1.3 1999/09/30 17:23:33 gray Exp $
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
#ifndef _Signal_hh
#define _Signal_hh

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadQueue.hh>
#include <signal.h>
#include <vector>
#include <map>

namespace Prague
{

class sigerr {};

class Signal 
  //. a wrapper for the (POSIX) signal handling functions}
{
  class Notifier;
  typedef vector<Notifier *> nlist_t;
  typedef map<int, nlist_t> dict_t;
public:
  class Notifier
    {
    public:
      virtual ~Notifier() {}
      virtual void notify(int) = 0;
    };
  enum type { hangup = SIGHUP, interrupt = SIGINT, quit = SIGQUIT, illegal = SIGILL,
	      trap = SIGTRAP, abort = SIGABRT, iotrap = SIGIOT, bus = SIGBUS, fpe = SIGFPE,
	      segv = SIGSEGV,
	      usr1 = SIGUSR1, usr2 = SIGUSR2, alarm = SIGALRM, terminate = SIGTERM, child = SIGCLD, io = SIGIO,
	      pipe = SIGPIPE, kill = SIGKILL};
  static bool set (int, Notifier *);
  //. add a notifier to be executed whenever the given signal is catched
  static bool unset (int, Notifier *);
  //. removes a notifier from the list for signum
  static void unset (int);
  //. remove all notifiers for the signal and reinstall the system's default handler
  static void mask (int);
  //. ignore the specified signal
  static void mask (int, int);
  //. block sigb while siga is handled
  static void unmask (int);
  //. don't ignore the specified signal any more
  static void unmask (int, int);
  static bool ispending (int);
  //. is there a pending signal of type @var{signum} (while being blocked)
  static sigset_t pending ();
  //. is there any pending signal (while being blocked)
  static void sysresume (int, bool);
  static char *name(int);
  //. returns the signal name of signum if nonzero or of the last signal beeing catched

  class Guard
  {
  public:
    Guard(int s) : signo(s) { mask(signo);}
    ~Guard() { unmask(signo);}
  private:
    int signo;
  };
private:
  static void notify (int);
  friend void sighandler (int signo);
  static void *run(void *);
  static dict_t notifiers;
  static Thread::Queue<int> queue;
  static Thread server;
};

};

#endif /* _Signal_hh */
