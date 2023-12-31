/*$Id: Coprocess.hh,v 1.8 1999/11/18 04:45:40 stefan Exp $
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
#ifndef _Coprocess_hh
#define _Coprocess_hh

#include <Prague/Sys/Signal.hh>
#include <Prague/Sys/Timer.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/IPC/Agent.hh>
#include <string>
#include <vector>

namespace Prague
{

class Coprocess : public Agent
{
  typedef vector<Coprocess *> plist_t;
  struct Reaper : Signal::Notifier { virtual void notify(int);};
  friend struct Reaper;
public:
  struct IONotifier
  {
    virtual ~IONotifier(){}
    virtual bool notify(iomask_t) = 0;
  };
  struct EOFNotifier
  {
    virtual ~EOFNotifier(){}
    virtual void notify(iomask_t) = 0;
  };
  enum state_t {ready, running, exited, signaled};
  Coprocess(const string &, IONotifier *, EOFNotifier * = 0);
  virtual      ~Coprocess();
  virtual void start();
  virtual void stop();
  const string &command() const { return path;}
  pid_t         pid() const { MutexGuard guard(mutex); return id;}
  state_t       state() const { MutexGuard guard(mutex); return _state;}
  int           value() const { MutexGuard guard(mutex); return _value;}
  void          timeout(long t, long h, long k) { _timeout.terminate = t, _timeout.hangup = h, _timeout.kill = k;}
  virtual ipcbuf *ibuf() { return inbuf;}
  virtual ipcbuf *obuf() { return outbuf;}
  virtual ipcbuf *ebuf() { return errbuf;}
protected:
  virtual bool processIO(int, iomask_t);
  void  terminate();
  void  shutdown(short);  
protected:
  string       path;
  IONotifier  *ioNotifier;
  EOFNotifier *eofNotifier;
  pid_t        id;
  state_t      _state;
  int          _value;
  ipcbuf      *inbuf;
  ipcbuf      *outbuf;
  ipcbuf      *errbuf;
private:
  Coprocess(const Coprocess &);
  Coprocess &operator = (const Coprocess &);
  void kill(int);
  mutable Mutex  mutex;
  struct
  {
    long hangup;
    long terminate;
    long kill;
  } _timeout;
  
  static plist_t processes;
  static Reaper  reaper;
  static Mutex   singletonMutex;
};

};

#endif /* _Coprocess_hh */
