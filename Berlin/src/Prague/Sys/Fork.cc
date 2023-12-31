/*$Id: Fork.cc,v 1.2 1999/07/23 21:06:11 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this file is based on code from the socket++ library
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
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

#include <Prague/Sys/Fork.hh>
#include <sys/wait.h>
#include <errno.h>
#include <cstdio>

using namespace Prague;

struct Fork::Process
{
  struct Cleaner { ~Cleaner ();};
  struct Reaper : Signal::Notifier { virtual void notify(int);};
  struct Suicide : Signal::Notifier { virtual void notify(int);};
  friend Cleaner;
  
  static void infanticideReason(pid_t, int);
  static Cleaner  cleaner;
  static Reaper   reaper;
  static Suicide  suicide;
  static Process *children;
  pid_t           pid;
  const bool      killflag : 1;
  const bool      reason : 1;
  Process        *next;
  
  Process(bool, bool);
  ~Process();
  
  void           kill() const;
  void           reap_child () const;
};

Fork::Process *Fork::Process::children = 0;
Fork::Process::Cleaner Fork::Process::cleaner;
Fork::Process::Reaper Fork::Process::reaper;
Fork::Process::Suicide Fork::Process::suicide;

Fork::Process::Cleaner::~Cleaner()
  // First, kill all children whose kill_child flag is set.
  // Second, wait for other children to die.
{
  for (Process *p = Fork::Process::children; p; p = p->next)
    if (p->killflag) delete p;
  while (Fork::Process::children && wait (0) > 0);
}

Fork::Process::Process (bool k, bool r)
  : killflag(k), reason(r), next(0)
{
  if (children == 0) Signal::set(Signal::child, &reaper);
  pid = fork();
  if (pid > 0)
    {
      next = children;
      children = this;
    }
  else if (pid == 0)
    {
      Process *p = children;
      while (p)
	{
	  Process *n = p->next;
	  p->pid = 0;
	  delete p;
	  p = n;
	}
      children = 0;
      if (killflag) Signal::set(Signal::kill, &suicide);
    }
}

Fork::Process::~Process()
{
  if (pid > 0)
    {
      if (killflag) ::kill (pid, Signal::terminate);
      reap_child();
      if (children == this) children = children->next;
      else
	{
	  for (Process *p = children; p; p = p->next)
	    if (p->next == this)
	      {
		p->next = next;
		break;
	      }
	}
    }
}

void Fork::Process::kill() const
{
  if (pid > 0)
    {
      ::kill(pid, Signal::kill);
      reap_child();
    }
}

void Fork::Process::reap_child () const
{
  int status;
  if (pid > 0 && waitpid (pid, &status, 0) == pid && reason)
    infanticideReason (pid, status);
}

void Fork::Process::infanticideReason (pid_t pid, int status)
{
  if (pid <= 0) return;
  if (WIFSTOPPED (status))
    cerr << "process " << pid << " gets "
	 << sys_siglist[WSTOPSIG (status)] << endl;
  else if (WIFEXITED (status))
    cerr << "process " << pid << " exited with status "
	 << WEXITSTATUS (status) << endl;
  else if (WIFSIGNALED (status))
    cerr << "process " << pid << " got "
	 << sys_siglist[WTERMSIG (status)] << endl;
}

void Fork::Process::Reaper::notify(int signo)
{
  if (signo != SIGCHLD) return;
  int status;
  pid_t wpid;
  if ((wpid = waitpid (-1, &status, WNOHANG)) > 0)
    {
      Process *prev = 0;
      Process *cur  = children;
      while (cur)
	{
	  if (cur->pid == wpid)
	    {
	      cur->pid = -1;
	      if (prev) prev->next = cur->next;
	      else children = children->next;
	      if (cur->reason) infanticideReason (wpid, status);
	      delete cur;
	      break;
	    }
	  prev = cur;
	  cur  = cur->next;
	}
    }
}

void Fork::Process::Suicide::notify (int)
{
  // if this process has any children we kill them.
  Process *p = children;
  while (p)
    {
      Process *next = p->next;
      if (!p->killflag) // otherwise Process::~Process will take care
	::kill(p->pid, Signal::kill);
      delete p; // Process::~Process will call reap_child ().
      p = next;
    }
  exit (0x0f); 
}

Fork::Fork (bool kill, bool reason)
  : process (new Process (kill, reason)) {}
Fork::~Fork () { if (process->pid <= 0) delete process;}
bool  Fork::child() const { return process->pid == 0;}
bool  Fork::parent() const { return process->pid > 0;}
pid_t Fork::pid() const { return process->pid;}
void  Fork::suicideOnSignal (int signo)
  // commit suicide at the signal signo
{
  if (!Signal::set(signo, &Process::suicide))
    perror ("Fork: Cannot commit suicide with the specified signal");
}
