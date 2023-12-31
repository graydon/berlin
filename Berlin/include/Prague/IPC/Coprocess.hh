/*$Id: Coprocess.hh,v 1.15 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _Prague_Coprocess_hh
#define _Prague_Coprocess_hh

#include <Prague/Sys/Signal.hh>
#include <Prague/Sys/Timer.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/IPC/Agent.hh>
#include <string>
#include <vector>

namespace Prague
{

//. a Coprocess ia an Agent that spawns a child process and takes care for
//. the associated housekeeping
class Coprocess : public Agent
{
  typedef std::vector<Coprocess *> plist_t;
  struct Reaper : Signal::Notifier { virtual void notify(int);};
  friend struct Reaper;
public:
  struct IONotifier
  {
    virtual ~IONotifier(){}
    virtual bool notify(iomask) = 0;
  };
  struct EOFNotifier
  {
    virtual ~EOFNotifier(){}
    virtual void notify(iomask) = 0;
  };
  enum state_t {ready, running, exited, signaled};
  Coprocess(const std::string &, IONotifier *, EOFNotifier * = 0);
  virtual      ~Coprocess();
  virtual void start();
  virtual void stop();
  //. return the command of the process being run
  const std::string &command() const { return _path;}
  //. return the process id of the child process
  pid_t         pid() const { Prague::Guard<Mutex> guard(_mutex); return _id;}
  //. return the state of the child process
  state_t       state() const { Prague::Guard<Mutex> guard(_mutex); return _state;}
  //. return the return value of the child process
  int           value() const { Prague::Guard<Mutex> guard(_mutex); return _value;}
  //. set timeout values used for the terminate call
  void          timeout(long t, long h, long k) { _timeout.terminate = t, _timeout.hangup = h, _timeout.kill = k;}
  virtual ipcbuf *ibuf() { return _inbuf;}
  virtual ipcbuf *obuf() { return _outbuf;}
  virtual ipcbuf *ebuf() { return _errbuf;}
protected:
  virtual bool process(int, iomask);
  void  terminate();
  void  shutdown(int);
protected:
  std::string  _path;
  IONotifier  *_ioNotifier;
  EOFNotifier *_eofNotifier;
  pid_t        _id;
  state_t      _state;
  int          _value;
  ipcbuf      *_inbuf;
  ipcbuf      *_outbuf;
  ipcbuf      *_errbuf;
private:
  Coprocess(const Coprocess &);
  Coprocess &operator = (const Coprocess &);
  bool terminated;
  void kill(int);
  mutable Mutex _mutex;
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

#endif
