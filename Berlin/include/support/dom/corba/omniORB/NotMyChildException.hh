#ifndef __NotMyChildException_hh__
#define __NotMyChildException_hh__

#ifndef USE_omniORB_logStream
#define USE_omniORB_logStream
#endif

#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

#ifndef __OMNILC_H_EXTERNAL_GUARD__
#define __OMNILC_H_EXTERNAL_GUARD__
#include <omniORB2/omniLC.h>
#endif


#ifdef _LC_attr
#error "A local CPP macro _LC_attr has already been defined."
#else
#ifdef  USE_stub_in_nt_dll
#define _LC_attr _OMNIORB_NTDLL_IMPORT
#else
#define _LC_attr
#endif
#endif

#define NotMyChildException_IntfRepoID "IDL:NotMyChildException:1.0"

class NotMyChildException : public CORBA::UserException {
public:

  
  NotMyChildException() {};
  NotMyChildException(const NotMyChildException &);
  NotMyChildException & operator=(const NotMyChildException &);
  virtual ~NotMyChildException() {};
  size_t NP_alignedSize(size_t initialoffset) const;
  void operator>>= (NetBufferedStream &) const;
  void operator<<= (NetBufferedStream &);
  void operator>>= (MemBufferedStream &) const;
  void operator<<= (MemBufferedStream &);
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_NotMyChildException;


void operator<<=(CORBA::Any& _a, const NotMyChildException& _s);
void operator<<=(CORBA::Any& _a, NotMyChildException* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, NotMyChildException*& _sp);


#undef _LC_attr

#endif // __NotMyChildException_hh__
