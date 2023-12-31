#include <Prague/Sys/Thread.hh>
#include <iostream>
#include <unistd.h>

using namespace Prague;

void *task0(void *a)
{
  cout << "task 0 sleeping 5 sec..." << endl;
  sleep(5);
  exit(0);
  return a;
}

void *task1(void *a)
{
  cout << "task 1 sleeping 1 sec..." << endl;
  sleep(2);
  return a;
}

void *task2(void *)
{
  cout << "task 2 sleeping 1 sec..." << endl;
  sleep(1);
  cout << "task 2 exiting..." << endl;
  const char *r = "task 2 exited";
  Thread::exit((char *)r);
  return 0;
}

void *task3(void *)
{
  cout << "task 3 sleeping forever..." << endl;
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
  if (thread1.state() == Thread::canceled) cout << "thread 1 canceled" << endl;
  else cout << "thread 1 finished, return value is " << *static_cast<long *>(r) << endl;
  thread2.join(&r);
  if (thread2.state() == Thread::canceled) cout << "thread 2 canceled" << endl;
  else cout << "thread 2 finished, return value is '" << static_cast<char *>(r) << '\'' << endl;
  cout << "cancelling thread 3..." << endl; 
  thread3.cancel();
  thread3.join(&r);
  if (thread3.state() == Thread::canceled) cout << "thread 3 canceled" << endl;
  else cout << "thread 3 finished, return value is '" << static_cast<char *>(r) << '\'' << endl;
//   thread0.join(0);
  while (true) sleep(1);
}
