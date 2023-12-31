//
// $Id: ReactorImpl.cc,v 1.6 1999/09/30 17:23:34 gray Exp $
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

// Reactors are basically 2 different kinds of typesafe dispatch
// tables: async ones have their own thread and message queue, sync
// ones do not.

#include "Berlin/Logger.hh"
#include "Command/ReactorImpl.hh"
#include <strstream>
#include <algorithm>

using namespace Prague;

// this lets you toggle the run state of the Reactor.
void ReactorImpl::active(CORBA::Boolean r)
{
  // we have to lock on something.. might as well use this
  MutexGuard guard(mutex);
  if (r)
    {
      if (running);  // do nothing, already running
      else running = true;
    }
  else
    {
      if (running) running = false;
      else;      // do nothing, already stopped
    }
}

CORBA::Boolean ReactorImpl::active()
{
  MutexGuard guard(mutex);
  return running;
}



void ReactorImpl::accept(const Message &m)
{  
  Logger::log(Logger::command) << "ReactorImpl::accept accepted message";
  if (running)
    {
      // store a method-local command vector to avoid deadlocking if a command
      // tries to modify react_map. this happens more than you'd think.
      vector<Command_var> commands_to_execute;
      
      // select the commands which match this message
      mutex.lock();            
      commands_to_execute = commands[m.payload.type()];
      mutex.unlock();      
      
      // execute each command, unbinding on exception
      for(clist_t::iterator i = commands_to_execute.begin(); i != commands_to_execute.end(); i++)
	{
	  try
	    {
	      Logger::log(Logger::command) << "ReactorImpl::accept running command for type " << m.payload.type()->id() << endl;
	      (*i)->execute(m);
	    }
	  catch (...)
	    {
	      Logger::log(Logger::command) << "ReactorImpl::accept command failed, unbinding for " << m.payload.type()->id() << endl;
	      this->unbind(m.payload.type(), *i);
	    }
	}
    }
}


// this lets you toggle the run state of the Reactor.
void AsyncReactorImpl::active(CORBA::Boolean r)
{
  // we have to lock on something.. might as well use this
  mutex.lock();
  if (r)
    {
      if (running);      // do nothing, already running
      else
	{
	  this->start();
	  running = true;
	}
    }
  else
    {
      if (running) running = false;
      else;     // do nothing, already stopped
    }
  mutex.unlock();
  condition.signal();
}

CORBA::Boolean AsyncReactorImpl::active()
{
  MutexGuard guard(mutex);
  return running;
}

// this just slots a message into the queue and wakes up the local thread.
void AsyncReactorImpl::accept(const Message &m)
{
  mutex.lock();
  Logger::log(Logger::message) << "received message of type " << m.payload.type()->id() << endl;
  queue.push(m);
  mutex.unlock();
  condition.signal();
}


static void *AsyncReactorImpl::run(void *)
{
  while(true)
    { // thread lives in here.        
      Logger::log(Logger::message) << "sleeping on message queue" << endl;
      condition.wait(); // this unlocks the queue atomically while it sleeps
      while(!queue.empty() && running)
	{ // when it wakes up, the queue is re-locked
	  const Message m = queue.top();
	  CORBA::TypeCode_var ty = m.payload.type();
	  Logger::log(Logger::message) << "processing a new message of type " << ty->id() << endl; 
	  queue.pop();
	  mutex.unlock();
	  this->ReactorImpl::accept(m);
	}
    }
}

// this constructor is necessary to initialize the queue condition variable
// (which allows thread sleep/wakeup) with a reference to the queue mutex
// (which the cond variable manages locking/unlocking)
AsyncReactorImpl::AsyncReactorImpl() : queue_mutex(), queue_cond(&queue_mutex) {} 

// bind will not duplicate bindings. specifically, for 1 typecode
// and 1 command reference, a binding can be made at most once.

void ReactorImpl::bind(CORBA::TypeCode_ptr ty, Command_ptr c)
{  
  MutexGuard guard(mutex);
  Logger::log(Logger::command) << "ReactorImpl::bind binding new command to type " << ty->id() << endl;
  CORBA::TypeCode_var newTy = ty;
  Command_var com = Command::_duplicate(c);
  vector<Command_var>::iterator i = find(commands[newTy].begin(), commands[ty].end(), com); 
  if (i != commands[ty].end()) commands[newTy].push_back(com);
}


// this unbinds the 1 (and only) association between a typecode
// and a command_ptr in this reactor. it happens automatically
// when the command goes null and/or throws an exception

void ReactorImpl::unbind(CORBA::TypeCode_ptr ty, Command_ptr c)
{
  MutexGuard guard(mutex);
  Command_var cmd = Command::_duplicate(c);
  Logger::log(Logger::command) << "ReactorImpl::unbind unbinding command from type " << ty->id() << endl;
  std::remove(commands[ty].begin(), commands[ty].end(), cmd);
  // remove the vector altogether if it's empty. 
  dictionary_t::iterator i = commands.find(ty);
  if (i->second.begin() == i->second.end()) commands.erase(i);
}  


void ReactorImpl::copy_react_map_to(Reactor_ptr r)
{
  MutexGuard guard(mutex);
  for(dictionary_t::iterator i = commands.begin(); i != commands.end(); i++)
    for(clist_t::iterator j = i->second.begin(); j != i->second.end(); j++)
      r->bind(i->first, *j);
}

