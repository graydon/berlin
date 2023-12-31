/*$Id: Signal.hh,v 1.8 2001/03/21 06:28:22 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Prague_Signal_hh
#define _Prague_Signal_hh

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadQueue.hh>
#include <signal.h>
#include <vector>
#include <map>

namespace Prague
{

class sigerr {};

//. a wrapper for the POSIX signal handling functions.
class Signal 
{
public:
  class Notifier
    {
    public:
      virtual ~Notifier() {}
      virtual void notify(int) = 0;
    };
private:
  typedef std::vector<Notifier *> nlist_t;
  typedef std::map<int, nlist_t> dict_t;
public:
  enum type { hangup = SIGHUP, interrupt = SIGINT, quit = SIGQUIT, illegal = SIGILL,
	      trap = SIGTRAP, abort = SIGABRT, iotrap = SIGIOT, bus = SIGBUS, fpe = SIGFPE,
	      segv = SIGSEGV,
	      usr1 = SIGUSR1, usr2 = SIGUSR2, alarm = SIGALRM, terminate = SIGTERM, child = SIGCHLD, io = SIGIO,
	      pipe = SIGPIPE, kill = SIGKILL};
  //. add a notifier to be executed whenever the given signal is catched
  static bool set(int, Notifier *);
  //. removes a notifier from the list for signum
  static bool unset(int, Notifier *);
  //. remove all notifiers for the signal and reinstall the system's default handler
  static void unset(int);
  //. ignore the specified signal
  static void mask(int);
  //. block sigb while siga is handled
  static void mask(int, int);
  //. don't ignore the specified signal any more
  static void unmask(int);
  //. don't ignore the specified signal any more
  static void unmask(int, int);
  //. is there a pending signal of type signum (while being blocked)
  static bool ispending(int signum);
  //. is there any pending signal (while being blocked)
  static sigset_t pending();
  static void sysresume(int, bool);
  //. returns the signal name of signum if nonzero or of the last signal beeing catched
  static const char *name(int);

  //. a Signal Guard, that masks a given signal over its lifetime.
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

#endif
