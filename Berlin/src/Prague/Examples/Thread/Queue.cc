/*$Id: Queue.cc,v 1.5 2001/03/25 08:25:16 stefan Exp $
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
#include <Prague/Sys/ThreadQueue.hh>
#include <vector>
#include <iostream>
#include <unistd.h>
#include <string>

using namespace Prague;

int reading, writing;

class lostream
{
public:
  lostream() { mutex.lock();}
  ~lostream() { mutex.unlock();}
  template <class T>
  lostream &operator << (const T &t) { std::cout << t; return *this;}
  lostream & operator << (std::ostream & (func)(std::ostream &)) { func(std::cout); return *this;}
private:
  static Mutex mutex;
};

Mutex lostream::mutex;

int test_random()
{
  static Mutex mutex;
  Prague::Guard<Mutex> guard(mutex);
  return rand();
}

typedef Thread::Queue<std::string> Queue;

Queue messages(5);

class Worker
{
public:
  Worker(int i)
    : number(i), thread(start, this)
    {
      thread.start();
    }
private:
  int number;
  Thread thread;
  void read()
    {
      std::string msg = messages.pop();
      lostream() << "worker " << number << " reading (" << messages.size() << " messages left)" << std::endl;
    }
  static void *start(void *X)
    {
      Worker *worker = reinterpret_cast<Worker *>(X);
      while (1)
	{
	  worker->read();
	  Thread::delay(Time(test_random() % reading));
	}
      return 0;
    }
};

int main(int argc, char **argv)
{
  if (argc != 3)
    {
      std::cout << "Usage : " << argv[0] << " <msg interval> <read interval> " << std::endl;
      return -1;
    }
  writing = atoi(argv[1]);
  reading = atoi(argv[2]);
  std::vector<Worker *> workers(10);
  for (size_t i = 0; i != workers.size(); i++) workers[i] = new Worker(i);
  for (int i = 0; i != 100; i++)
    {
      messages.push("hi there");
      lostream() << "main writing (" << messages.size() << " messages in queue)" << std::endl;
      Thread::delay(Time(test_random() % writing));
    }
  for (size_t i = 0; i != workers.size(); i++) delete workers[i];
}
