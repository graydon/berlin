/*$Id: Data.cc,v 1.2 2000/03/22 22:30:17 stefan Exp $
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

#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadData.hh>
#include <vector>
#include <iostream>
#include <unistd.h>
#include <string>

using namespace Prague;

class lostream
{
public:
  lostream() { mutex.lock();}
  ~lostream() { mutex.unlock();}
  template <class T>
  lostream &operator << (const T &t) { cout << t; return *this;}
  lostream &operator << (ostream & (func)(ostream &)) { func(cout); return *this;}
private:
  static Mutex mutex;
};

Mutex lostream::mutex;

class Worker
{
public:
  Worker() : thread(&Worker::start, this) { thread.start();}
  ~Worker() { thread.join(0);}
private:
  static int counter;
  static Mutex mutex;
  static Thread::Data<int> tsd;
  Thread thread;
  static void *start(void *)
    {
      mutex.lock();
      tsd = counter++;
      mutex.unlock();
      lostream() << "thread creates specific data: " << *tsd << endl;
      Thread::delay(Time(200));
      lostream() << "thread destroys specific data: " << *tsd << endl;
      return 0;
    }
};

int   Worker::counter = 0;
Mutex Worker::mutex;
Thread::Data<int> Worker::tsd;

int main(int argc, char **argv)
{
  vector<Worker *> workers(10);
  for (size_t i = 0; i != workers.size(); i++) workers[i] = new Worker();
  for (size_t i = 0; i != workers.size(); i++) delete workers[i];
}
