#ifndef _ReactorImpl_hh
#define _ReactorImpl_hh

//
// $Id: ReactorImpl.hh,v 1.5 1999/09/30 17:23:33 gray Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//


#include "Warsaw/config.hh"
#include "Warsaw/Message.hh"
#include "Berlin/CloneableImpl.hh"
#include "Warsaw/CommandKit.hh"
#include "Prague/Sys/Thread.hh"
#include "Prague/Sys/ThreadQueue.hh"

#include <map>
#include <vector>

// this is a message listener whose behaviour you can modify by
// binding command objects to different typecodes, where the typecode
// is a "match" if the payload in a received message is the same as
// the binding.

class ReactorImpl : implements(Reactor)
{  
  typedef vector<Command_var> clist_t;
  typedef map<CORBA::TypeCode_var, clist_t> dictionary_t; 
public: 
  virtual void accept(const Message &m);
  virtual CORBA::Boolean active();
  virtual void active(CORBA::Boolean);
  void bind(CORBA::TypeCode_ptr tc, Command_ptr c);
  void unbind(CORBA::TypeCode_ptr tc, Command_ptr c);
  
private:
  // just copies the reactor map. a little helper function
  void copy_react_map_to(Reactor_ptr r);
  bool running;
  
  // the command dispatch table
  dictionary_t commands;
  Prague::Mutex mutex;  
};
    

// this just serves as a comparator in the asyncReactor's priority
// queue. In typical unix backwardness, the "lowest" priority (neg
// 65536) and the lowest time (time 0) will come to the head of the
// queue fastest. that's how it works. live with it.

class messageCmp {
    public:
    inline bool operator()(const Message &a, const Message &b) const {
	return (a.timeOfCreation > b.timeOfCreation) &&
	    (a.priority > b.priority);
    }
};



// this is a reactor which contains its own thread which does the
// actual command lookup and execute. means when you send it a
// message it'll return in constant, small time, and your thread
// won't get all tangled up in affairs it's not interested in.

class AsyncReactorImpl : public ReactorImpl
{
  
public:
  AsyncReactorImpl();
  virtual void accept(const Message &m);
  virtual CORBA::Boolean active();
  virtual void active(CORBA::Boolean);
protected:
    
  // the incoming message queue
//   priority_queue<Message, vector<Message>, messageCmp> react_queue; 
  
  // thread synchronization items
  
  // overrides from omnithread
  static void *run(void *); 
  Prague::Thread thread;
  Prague::Thread::Queue<Message> queue;
  Prague::Mutex mutex;
  Prague::Condition condition;
};

#endif
