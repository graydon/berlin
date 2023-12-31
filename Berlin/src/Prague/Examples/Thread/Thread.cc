#include <Prague/Sys/Thread.hh>
#include <iostream>
#include <unistd.h>

using namespace Prague;

struct DebugGuard
{
  DebugGuard() { std::cout << "DebugGuard::DebugGuard" << std::endl;}
  ~DebugGuard() { std::cout << "DebugGuard::~DebugGuard" << std::endl;}
};

void *task0(void *a)
{
  std::cout << "task 0 sleeping 5 sec..." << std::endl;
  sleep(5);
  std::cout << "exiting..." << std::endl;
  exit(0);
  return a;
}

void *task1(void *a)
{
  std::cout << "task 1 sleeping 1 sec..." << std::endl;
  sleep(2);
  return a;
}

void *task2(void *)
{
  std::cout << "task 2 sleeping 1 sec..." << std::endl;
  sleep(1);
  std::cout << "task 2 exiting..." << std::endl;
  const char *r = "task 2 exited";
  Thread::exit((char *)r);
  return 0;
}

void *task3(void *)
{
  std::cout << "task 3 sleeping forever..." << std::endl;
  DebugGuard guard;
  while (true)
    {
      sleep(1);
      Thread::testcancel();
    }
  return 0;
}

Thread thread0(task0, 0);

int main(int, char **)
{
  long l = 42;
  Thread thread1(task1, &l);
  Thread thread2(task2, &l);
  Thread thread3(task3, &l);
  Thread thread4(task3, &l); // never start...
  thread0.start();
  thread1.start();
  thread2.start();
  thread3.start();
  void *r;
  thread1.join(&r);
  if (thread1.state() == Thread::canceled) std::cout << "thread 1 canceled" << std::endl;
  else std::cout << "thread 1 finished, return value is " << *static_cast<long *>(r) << std::endl;
  thread2.join(&r);
  if (thread2.state() == Thread::canceled) std::cout << "thread 2 canceled" << std::endl;
  else std::cout << "thread 2 finished, return value is '" << static_cast<char *>(r) << '\'' << std::endl;
  std::cout << "cancelling thread 3..." << std::endl; 
  thread3.cancel();
  thread3.join(&r);
  if (thread3.state() == Thread::canceled) std::cout << "thread 3 canceled" << std::endl;
  else std::cout << "thread 3 finished, return value is '" << static_cast<char *>(r) << '\'' << std::endl;
//   thread0.join(0);
  while (true) sleep(1);
}
