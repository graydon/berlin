/*$Id: Signal.cc,v 1.5 1999/11/16 02:15:20 stefan Exp $
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
#include "Prague/Sys/Signal.hh"
#include <algorithm>
#include <cstdio>

namespace Prague
{
typedef void (*sighnd_type) (...);
void sighandler (int signo)
{
  /*
   * due to the non conformance of LinuxThreads with
   * POSIX 1003.1c we can't let the signal server thread
   * handle the SIGCHLD signal. 'wait()' would fail.
   * Instead, we have to handle it directly, trusting
   * the event handler that it is reentrant and async safe
   * - stefan
   */
  if (signo == Signal::child) Signal::notify(signo);
  else if (!Thread::id())
    Signal::queue.push(signo);
}

};

using namespace Prague;

Signal::dict_t     Signal::notifiers;
Thread::Queue<int> Signal::queue(32);
Thread             Signal::server(&Signal::run, 0);
extern "C" char *strsignal(int signo);

bool Signal::set (int signum, Signal::Notifier *notifier)
{
  if (server.state() != Thread::running) server.start();
  if (!notifiers[signum].size())
    {
      struct sigaction sa;
      if (sigaction (signum, 0, &sa) == -1) return false;
      if (sa.sa_handler != sighnd_type (&sighandler))
	{
	  sa.sa_handler = sighnd_type (&sighandler);
	  if (sigemptyset (&sa.sa_mask) == -1) return false;
	  sa.sa_flags = 0;
	  if (sigaction (signum, &sa, 0) == -1) return false;
	}
    }
  notifiers[signum].push_back(notifier);
  return true;
}

bool Signal::unset (int signum, Signal::Notifier *notifier)
{
  nlist_t::iterator i = find (notifiers[signum].begin(), notifiers[signum].end(), notifier);
  if (i != notifiers[signum].end())
    {
      notifiers[signum].erase(i);
      if (!notifiers[signum].size()) unset(signum);
      return true;
    }
  return false;
}

void Signal::unset (int signum)
{
  notifiers[signum].erase(notifiers[signum].begin(), notifiers[signum].end());
  struct sigaction sa;
  if (sigaction (signum, 0, &sa) == -1) return;
  if (sa.sa_handler == sighnd_type (&sighandler))
    {
      sa.sa_handler = sighnd_type (SIG_DFL);
      if (sigemptyset (&sa.sa_mask) == -1) return;
      sa.sa_flags = 0;
      if (sigaction (signum, &sa, 0) == -1) return;
    }
}

void Signal::mask (int signum)
{
  sigset_t s;
  if (sigemptyset (&s) == -1) return;
  if (sigaddset (&s, signum) == -1) return;
  if (sigprocmask (SIG_BLOCK, &s, 0) == -1) return;
}

void Signal::mask (int siga, int sigb)
{
  struct sigaction sa;
  if (sigaction (siga, 0, &sa) == -1) return;
  if (sa.sa_handler != sighnd_type (&sighandler))
    {
      sa.sa_handler = sighnd_type (&sighandler);
      if (sigemptyset (&sa.sa_mask) == -1) return;
      sa.sa_flags = 0;
    }
  if (sigaddset (&sa.sa_mask, sigb) == -1) return;
  if (sigaction (siga, &sa, 0) == -1) return;
}

void Signal::unmask (int signum)
{
  sigset_t s;
  if (sigemptyset (&s) == -1) return;
  if (sigaddset (&s, signum) == -1) return;
  if (sigprocmask (SIG_UNBLOCK, &s, 0) == -1) return;
}

void Signal::unmask (int siga, int sigb)
{
  struct sigaction sa;
  if (sigaction (siga, 0, &sa) == -1) return;
  if (sa.sa_handler != sighnd_type (&sighandler))
    {
      sa.sa_handler = sighnd_type (&sighandler);
      if (sigemptyset (&sa.sa_mask) == -1) return;
      sa.sa_flags = 0;
    }
  else { if (sigdelset (&sa.sa_mask, sigb) == -1) return;}
  if (sigaction (siga, &sa, 0) == -1) return;
}

void Signal::sysresume (int signum, bool set)
{
  struct sigaction sa;
  if (sigaction (signum, 0, &sa) == -1) return;
  if (sa.sa_handler != sighnd_type (&sighandler))
    {
      sa.sa_handler = sighnd_type (&sighandler);
      if (sigemptyset (&sa.sa_mask) == -1) return;
      sa.sa_flags = 0;
    }
  if (sigaction (signum, &sa, 0) == -1) return;
}

sigset_t Signal::pending ()
{
  sigset_t s;
  if (sigemptyset (&s) == -1) return s;
  if (sigpending (&s) == -1) return s;
  return s;
}

bool Signal::ispending (int signo)
{
  sigset_t s = pending();
  switch (sigismember (&s, signo))
    {
    case  1: return true;
    case -1: perror("Signal::ispending: ");
    case  0:
    default: return false;
    }
  return false;
}

char *Signal::name(int signum)
{
  return strsignal(signum);
}

struct execute
{
  execute(int s) : signum(s) {}
  void operator () (Signal::Notifier *notifier) { notifier->notify(signum);}
  int signum;
};

void Signal::notify(int signum)
{
  nlist_t tmp = notifiers[signum];
  execute e(signum);
  for_each(tmp.begin(), tmp.end(), e);
}

void *Signal::run(void *)
{
  while (true)
    {
      int signo = Signal::queue.pop();
      Signal::notify(signo);
      Thread::testcancel();
    }
  return 0;
}
