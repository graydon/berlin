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
  lostream &operator << (const T &t) { std::cout << t; return *this;}
  lostream & operator << (std::ostream & (func)(std::ostream &)) { func(std::cout); return *this;}
private:
  static Mutex mutex;
};

int test_random()
{
  static Mutex mutex;
  Prague::Guard<Mutex> guard(mutex);
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
      Prague::Guard<Mutex> guard(mutex);
      running = true;
      server.start();
    }
  void cancel()
    {
      {
	Prague::Guard<Mutex> guard(mutex);
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
  std::vector<Philosopher *> philosophers;
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
      lostream() << "Philosopher #" << seat << " has entered the room." << std::endl;
    }
  ~Philosopher()
    {
      lostream() << "Philosopher #" << seat << " has left the room." << std::endl;
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
  static std::vector<Mutex> chopsticks;
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
  Prague::Guard<Mutex> guard(mutex);      
  delete philosophers[i];
  philosophers[i] = 0;
  occupancy--;
  condition.signal();
}

void Philosopher::start()
{
  int left = seat;
  int right = left == Diner::seats - 1 ? 0 : left + 1;
  if (left % 1) std::swap(left, right);
  int count = test_random() % 10 + 1;
  while (count--)
    {
      chopsticks[left].lock();
      chopsticks[right].lock();
      lostream() << "Philosopher #" << seat << " is eating spaghetti now." << std::endl;
      Thread::delay(Time(test_random() % 2000));
      chopsticks[left].unlock();
      chopsticks[right].unlock();
      lostream() << "Philosopher #" << seat << " is pondering about life." << std::endl;
      Thread::delay(Time(test_random() % 2000));
    }
  diner->remove(seat);
}

Mutex         lostream::mutex;
std::vector<Mutex> Philosopher::chopsticks(Diner::seats);

int main(int argc, char *argv)
{
  Diner diner;
  diner.run();
  while (diner.active()) sleep(1);
}
