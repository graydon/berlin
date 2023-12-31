/*$Id: Diner.cc,v 1.2 1999/09/30 17:23:34 gray Exp $
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

#include <Prague/Sys/Thread.hh>
#include <vector>
#include <iostream>
#include <unistd.h>

using namespace Prague;

class lostream
{
public:
  lostream() { mutex.lock();}
  ~lostream() { mutex.unlock();}
  template <class T>
  lostream &operator << (const T &t) { cout << t; return *this;}
  lostream & operator << (ostream & (func)(ostream &)) { func(cout); return *this;}
private:
  static Mutex mutex;
};

int random()
{
  static Mutex mutex;
  MutexGuard guard(mutex);
  return rand();
}

class Philosopher;

class Diner
{
public:
  static const int seats = 5;
  Diner() : server(proc, this), condition(mutex), running(false) {}
  ~Diner() { cancel();}
  void run()
    {
      MutexGuard guard(mutex);
      running = true;
      server.start();
    }
  void cancel()
    {
      {
	MutexGuard guard(mutex);
	running = false;
      }
      server.join(0);
    }
  bool active() { return running;}
  void remove(int i);
private:
  static void *proc(void *X)
    {
      Diner *diner = reinterpret_cast<Diner *>(X);
      diner->start();
      return 0;
    }
  void start();
  vector<Philosopher *> philosophers;
  Thread server;
  Mutex mutex;
  Condition condition;
  bool running;
  int occupancy;
};

class Philosopher
{
public:
  Philosopher(int i, Diner *d) : thread(proc, this), seat(i), diner(d)
    {
      thread.start();
      lostream() << "Philosopher #" << seat << " has entered the room." << endl;
    }
  ~Philosopher()
    {
      lostream() << "Philosopher #" << seat << " has left the room." << endl;
    }
private:
  static void *proc(void *X)
    {
      Philosopher *philosopher = reinterpret_cast<Philosopher *>(X);
      philosopher->start();
      return 0;
    }
  void start();
  Thread thread;
  int seat;
  Diner *diner;
  static vector<Mutex> chopsticks;
};

void Diner::start()
{
  mutex.lock();
  for (int i = 0; i < seats; i++) philosophers.push_back(new Philosopher(i, this));
  occupancy = seats;
  while (running)
    {
      while (occupancy == seats) condition.wait();
//       mutex.unlock();
//       Thread::delay(Time(1000));
//       mutex.lock();
      for (int i = 0; i < seats; i++)
	{
	  if (!philosophers[i])
	    {
	      philosophers[i] = new Philosopher(i, this);
	      occupancy++;
	    }
	}
    }
}

void Diner::remove(int i)
{
  MutexGuard guard(mutex);      
  delete philosophers[i];
  philosophers[i] = 0;
  occupancy--;
  condition.signal();
}

void Philosopher::start()
{
  int left = seat;
  int right = left == Diner::seats - 1 ? 0 : left + 1;
  if (left % 1) swap(left, right);
  int count = random() % 10 + 1;
  while (count--)
    {
      chopsticks[left].lock();
      chopsticks[right].lock();
      lostream() << "Philosopher #" << seat << " is eating spaghetti now." << endl;
      Thread::delay(Time(random() % 2000));
      chopsticks[left].unlock();
      chopsticks[right].unlock();
      lostream() << "Philosopher #" << seat << " is pondering about life." << endl;
      Thread::delay(Time(random() % 2000));
    }
  diner->remove(seat);
}

Mutex         lostream::mutex;
vector<Mutex> Philosopher::chopsticks(Diner::seats);

int main(int argc, char *argv)
{
  Diner diner;
  diner.run();
  while (diner.active()) sleep(1);
}
