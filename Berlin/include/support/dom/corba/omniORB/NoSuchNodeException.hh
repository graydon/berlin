#ifndef __NoSuchNodeException_hh__
#define __NoSuchNodeException_hh__

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


_CORBA_MODULE DOM

_CORBA_MODULE_BEG

#define DOM_NoSuchNodeException_IntfRepoID "IDL:DOM/NoSuchNodeException:1.0"

  class NoSuchNodeException : public CORBA::UserException {
  public:

    
    NoSuchNodeException() {};
    NoSuchNodeException(const NoSuchNodeException &);
    NoSuchNodeException & operator=(const NoSuchNodeException &);
    virtual ~NoSuchNodeException() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  _CORBA_MODULE_VAR const CORBA::TypeCode_ptr _tc_NoSuchNodeException;


_CORBA_MODULE_END


void operator<<=(CORBA::Any& _a, const DOM::NoSuchNodeException& _s);
void operator<<=(CORBA::Any& _a, DOM::NoSuchNodeException* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, DOM::NoSuchNodeException*& _sp);


#undef _LC_attr

#endif // __NoSuchNodeException_hh__
