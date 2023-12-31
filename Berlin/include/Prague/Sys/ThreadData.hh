/*$Id: ThreadData.hh,v 1.3 2000/03/22 22:30:16 stefan Exp $
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
#ifndef _Prague_ThreadData_hh
#define _Prague_ThreadData_hh

#include <Prague/Sys/Thread.hh>

namespace Prague
{

template <class T>
class Thread::Data
{
public:
  class TooMuchTSDs : public Exception { public: TooMuchTSDs() : Exception("too much TSDs defined") {}};
  class InvalidKey : public Exception { public: InvalidKey() : Exception("illegal key") {}};

  Data(T v) throw (TooMuchTSDs, InvalidKey) : initializer(v)
    {
      if(pthread_key_create(&key, destructor)) throw TooMuchTSDs();
      if(pthread_setspecific(key, new T(initializer))) throw InvalidKey();
    }
  Data() throw (TooMuchTSDs, InvalidKey)
    {
      if(pthread_key_create(&key, destructor)) throw TooMuchTSDs();
      if(pthread_setspecific(key, new T)) throw InvalidKey();
    }
  ~Data()
    {
      delete reinterpret_cast<T *>(pthread_getspecific(key));
      pthread_key_delete(key);
    }
  const T &var(void) const throw (InvalidKey)
    {
      T *data = reinterpret_cast<T *>(pthread_getspecific(key));
      if (!data)
	{
	  if (pthread_setspecific(key, new T(initializer))) throw InvalidKey();
	  data = reinterpret_cast<T *>(pthread_getspecific(key));
	}
      return *data;
    }
  T &var(void) throw (InvalidKey)
    {
      T *data = reinterpret_cast<T *>(pthread_getspecific(key));
      if (!data)
	{
	  if (pthread_setspecific(key, new T(initializer))) throw InvalidKey();
	  data = reinterpret_cast<T *>(pthread_getspecific(key));
	}
      return *data;
    }
  Data<T> &operator = (const T &t) throw (InvalidKey)
    {
      T *data = reinterpret_cast<T *>(pthread_getspecific(key));
      if (data) *data = t;
      else if (pthread_setspecific(key, new T(t))) throw InvalidKey();
      return *this;
    }
  Data<T> &operator = (const Data<T> &t) throw (InvalidKey)
    {
      T *data = reinterpret_cast<T *>(pthread_getspecific(key));
      if (data) *data = *t;
      else if (pthread_setspecific(key, new T(*t))) throw InvalidKey();
      return *this;
    }
  const T *operator->() const throw (InvalidKey)
    {
      T *data = reinterpret_cast<T *>(pthread_getspecific(key));
      if (!data)
	{
	  if (pthread_setspecific(key, new T(initializer))) throw InvalidKey();
	  data = reinterpret_cast<T *>(pthread_getspecific(key));
	}
      return data;
    }
  T *operator->() throw (InvalidKey)
    {
      T *data = reinterpret_cast<T *>(pthread_getspecific(key));
      if (!data)
	{
	  if (pthread_setspecific(key, new T(initializer))) throw InvalidKey();
	  data = reinterpret_cast<T *>(pthread_getspecific(key));
	}
      return data;
    }
  const T &operator *() const { return var();}
        T &operator *()       { return var();}
private:
  static void destructor(void *data) { delete reinterpret_cast<T *>(data);}
  Data(const Data<T> &); 
  pthread_key_t key;
  T initializer;
};

};

#endif /* _Prague_ThreadData_hh */
