//
// $id:$
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 The Berlin Consortium 
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
// Authors:
// ANOQ of the Sun (anoq@berlin-consortium.org or anoq@vip.cybercity.dk)
// Sai Lai Lo (sll)

// Note: Most of this code is ripped off from omniORB's include
//       files. The original copyrightnotice follows:

//                            Package   : omniORB2
// CORBA.h                    Created on: 30/1/96
//                            Author    : Sai Lai Lo (sll)
//
//    Copyright (C) 1996, 1997 Olivetti & Oracle Research Laboratory
//
//    This file is part of the omniORB library
//
//    The omniORB library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  
//    02111-1307, USA
//
//
// Description:
// 	A complete set of C++ definitions for the CORBA module. The definitions
//	appear within the C++ class named CORBA. This mapping is fully
//      compliant with the CORBA 2.0 specification.
//

#ifndef __NOCORBA__
#define __NOCORBA__

#ifndef SIZEOF_LONG
#define SIZEOF_LONG 4
#endif

#ifndef SIZEOF_INT
#define SIZEOF_INT 4
#endif

#ifndef SIZEOF_PTR
#define SIZEOF_PTR  4
#endif

#ifdef HAS_Cplusplus_Bool
typedef bool                      _CORBA_Boolean;
#else
typedef unsigned char             _CORBA_Boolean;
#endif

typedef unsigned char             _CORBA_Char;

typedef unsigned char             _CORBA_Octet;

typedef short                     _CORBA_Short;

typedef unsigned short            _CORBA_UShort;

#if SIZEOF_LONG == 4
typedef long                      _CORBA_Long;

typedef unsigned long             _CORBA_ULong;
#elif SIZEOF_INT == 4
typedef int                       _CORBA_Long;

typedef unsigned int              _CORBA_ULong;
#else
# error "Can't map Long (32 bits) to a native type."
#endif

class CORBA
{
public:
  typedef _CORBA_Boolean Boolean;
  typedef _CORBA_Char    Char;
  typedef _CORBA_Octet   Octet;
  typedef _CORBA_Short   Short;
  typedef _CORBA_UShort  UShort;
  typedef _CORBA_Long    Long;
  typedef _CORBA_ULong   ULong;

  class Object
  {};
  typedef Object *Object_ptr;
  typedef Object_ptr ObjectRef;

  class Exception {
  public:
    virtual ~Exception() {}
  protected:
    Exception() {}
  };

  enum CompletionStatus { COMPLETED_YES, COMPLETED_NO, COMPLETED_MAYBE };
  class SystemException : public Exception {
  public:
    SystemException() {
      pd_minor = 0;
      pd_status = COMPLETED_NO;
    }

    SystemException(const SystemException &e) {
      pd_minor = e.pd_minor;
      pd_status = e.pd_status;
    }

    SystemException(ULong minor, CompletionStatus status) {
      pd_minor = minor;
      pd_status = status;
      return;
    }

    virtual ~SystemException() {}

    SystemException &operator=(const SystemException &e) {
      pd_minor = e.pd_minor;
      pd_status = e.pd_status;
      return *this;
    }

#ifdef minor
    // Digital Unix 3.2, and may be others as well, defines minor() as
    // a macro in its sys/types.h. Get rid of it!
#undef minor
#endif

    ULong minor() const { return pd_minor; }

    void minor(ULong m) { pd_minor = m; return; }

    CompletionStatus completed() const { return pd_status; }

    void completed(CompletionStatus s) { pd_status = s; return; }

    virtual const char * NP_RepositoryId() const { return ""; }

  protected:
    ULong             pd_minor;
    CompletionStatus  pd_status;
  };

#define  STD_EXCEPTION(name) \
  class name : public SystemException { \
  public: \
       name (ULong minor = 0, CompletionStatus completed = COMPLETED_NO \
       ) : SystemException (minor,completed) {} \
  }

  STD_EXCEPTION (NO_MEMORY);
};

inline void
_CORBA_new_operator_return_null()
{
  throw CORBA::NO_MEMORY(0,CORBA::COMPLETED_NO);
}

template <class T>
class _CORBA_Sequence {
public:
  inline _CORBA_Sequence() : pd_max(0), pd_len(0), pd_rel(1), pd_buf(0) { }
  inline _CORBA_Sequence(_CORBA_ULong max) :
             pd_max(max), pd_len(0), pd_rel(1)
  {
    if (!(pd_buf = allocbuf(max))) {
      _CORBA_new_operator_return_null();
      // never reach here
    }
    return;
  }

  inline _CORBA_Sequence(_CORBA_ULong max,
			 _CORBA_ULong length,
			 T           *value,
			 _CORBA_Boolean release = 0) 
      : pd_max(max), 
	pd_len(length), 
	pd_rel(release),
	pd_buf(value)
  {
    if (length > max) {
      _CORBA_bound_check_error();
      // never reach here
    }
    return;
  }

  inline _CORBA_Sequence(const _CORBA_Sequence<T>& s)
              : pd_max(s.pd_max), 
		pd_len(s.pd_len),
		pd_rel(1)
  {
    if (!(pd_buf = allocbuf(s.pd_max))) {
      _CORBA_new_operator_return_null();
      // never reach here
    }
    for (_CORBA_ULong i=0; i < s.pd_len; i++) {
      pd_buf[i] = s.pd_buf[i];
    }
  }

  inline ~_CORBA_Sequence() {
    if (pd_rel && pd_buf) freebuf(pd_buf);
    pd_buf = 0;
    return;
  }
  inline _CORBA_Sequence<T> &operator= (const _CORBA_Sequence<T> &s)
  {
    if (pd_max < s.pd_max)
      {
	T *newbuf = allocbuf(s.pd_max);
	if (!newbuf) {
	  _CORBA_new_operator_return_null();
	  // never reach here
	}
	pd_max = s.pd_max;
	if (pd_rel && pd_buf) {
	  freebuf(pd_buf);
	}
	else {
	  pd_rel = 1;
	}
	pd_buf = newbuf;
      }
    pd_len = s.pd_len;
    for (unsigned long i=0; i < pd_len; i++) {
      pd_buf[i] = s.pd_buf[i];
    }
    return *this;
  }

  inline _CORBA_ULong maximum() const { return pd_max; }
  inline _CORBA_ULong length() const { return pd_len; }
  inline void length(_CORBA_ULong length)
  {
    if (length > pd_max)
      {
	T *newbuf = allocbuf(length);
	if (!newbuf) {
	  _CORBA_new_operator_return_null();
	  // never reach here
	}
	for (unsigned long i=0; i < pd_len; i++) {
	  newbuf[i] = pd_buf[i];
	}
	pd_max = length;
	if (pd_rel && pd_buf) {
	  freebuf(pd_buf);
	}
	else {
	  pd_rel = 1;
	}
	pd_buf = newbuf;
      }
    pd_len = length;
    return;
  }
  inline T &operator[] (_CORBA_ULong index)
  {
    if (index >= length()) {
      _CORBA_bound_check_error();
    }
    return pd_buf[index];
  }
  inline const T &operator[] (_CORBA_ULong index) const
  {
    if (index >= length()) {
      _CORBA_bound_check_error();
    }
    return pd_buf[index];
  }
  static inline T* allocbuf(_CORBA_ULong nelems)
  {
    return new T[nelems];
  }
  static inline void freebuf(T * b)
  {
    if (b) delete [] b; 
    return;
  }
  // omniORB2 extensions
  inline T *NP_data() const { return pd_buf; }
  inline void NP_norelease() { pd_rel = 0; }
  //inline void operator>>= (NetBufferedStream &s) const;
  //inline void operator<<= (NetBufferedStream &s);
  //inline void operator>>= (MemBufferedStream &s) const;
  //inline void operator<<= (MemBufferedStream &s);
private:
  _CORBA_ULong    pd_max;
  _CORBA_ULong    pd_len;
  _CORBA_Boolean  pd_rel;
  T              *pd_buf;
};

template <class T,int elmSize,int elmAlignment>
class _CORBA_Unbounded_Sequence_w_FixSizeElement 
   : public _CORBA_Sequence<T> 
{
public:
  inline _CORBA_Unbounded_Sequence_w_FixSizeElement() {}
  inline _CORBA_Unbounded_Sequence_w_FixSizeElement(_CORBA_ULong max)
    : _CORBA_Sequence<T>(max) {}
  inline _CORBA_Unbounded_Sequence_w_FixSizeElement(_CORBA_ULong max,
						    _CORBA_ULong length,
						    T           *value,
						    _CORBA_Boolean release = 0)
    : _CORBA_Sequence<T>(max,length,value,release) {}
  inline _CORBA_Unbounded_Sequence_w_FixSizeElement (const 
       _CORBA_Unbounded_Sequence_w_FixSizeElement<T,elmSize,elmAlignment>& s)
    : _CORBA_Sequence<T>(s) {}
  inline ~_CORBA_Unbounded_Sequence_w_FixSizeElement() {}
  inline _CORBA_Unbounded_Sequence_w_FixSizeElement<T,elmSize,elmAlignment> &
      operator= 
        (const 
	  _CORBA_Unbounded_Sequence_w_FixSizeElement<T,elmSize,elmAlignment> &
	 s) 
  {
    _CORBA_Sequence<T>::operator= (s);
    return *this;
  }
  //inline size_t NP_alignedSize(size_t initialoffset) const;
  //inline void operator>>= (NetBufferedStream &s) const;
  //inline void operator<<= (NetBufferedStream &s);
  //inline void operator>>= (MemBufferedStream &s) const;
  //inline void operator<<= (MemBufferedStream &s);
};

template <class T,class ElmType>
class _CORBA_Sequence_Var {
public:
  typedef T* ptr_t;
  inline _CORBA_Sequence_Var() { pd_data = 0; }
  inline _CORBA_Sequence_Var(T* p) { pd_data = p; }
  inline _CORBA_Sequence_Var(const _CORBA_Sequence_Var<T,ElmType> &p);
  inline ~_CORBA_Sequence_Var() {  if (pd_data) delete pd_data; }
  inline _CORBA_Sequence_Var<T,ElmType> &operator= (T* p);
  inline _CORBA_Sequence_Var<T,ElmType> &operator= (const _CORBA_Sequence_Var<T,ElmType> &p);
  inline ElmType &operator[] (_CORBA_ULong index) { return (pd_data->_CORBA_Sequence<ElmType>::NP_data())[index]; }
  inline const ElmType &operator[] (_CORBA_ULong index) const {
    return (pd_data->_CORBA_Sequence<ElmType>::NP_data())[index];
  }
  inline T* operator->() const { return (T*)pd_data; }

#if defined(__GNUG__) && __GNUG__ == 2 && __GNUC_MINOR__ == 7
  inline operator T& () const { return *pd_data; }
#else
  inline operator const T& () const { return *pd_data; }
  inline operator T& () { return *pd_data; }
#endif
  // This conversion operator is necessary to support the implicit conversion
  // when this var type is used as the IN or INOUT argument of an operation.

  // The following coversion operators are needed to support the casting
  // of this var type to a const T* or a T*. The CORBA spec. doesn't say
  // these castings must be supported so they are deliberately left out.
  // In fact, the operator->() can always be used to get to the T*.
  //
  // inline operator const T* () const { return pd_data; }
  // inline operator T* () { return pd_data; }

  //friend class _CORBA_Sequence_OUT_arg<T,_CORBA_Sequence_Var<T,ElmType> >;

private:
  T* pd_data;
};

#endif //__NOCORBA__