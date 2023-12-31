#ifndef __ProcessingInstruction_hh__
#define __ProcessingInstruction_hh__

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

#ifndef __Node_EXTERNAL_GUARD__
#define __Node_EXTERNAL_GUARD__
#include <Node.hh>
#endif
#ifndef __domDefs_EXTERNAL_GUARD__
#define __domDefs_EXTERNAL_GUARD__
#include <domDefs.hh>
#endif
#ifndef __NoSuchAttributeException_EXTERNAL_GUARD__
#define __NoSuchAttributeException_EXTERNAL_GUARD__
#include <NoSuchAttributeException.hh>
#endif
#ifndef __NotMyChildException_EXTERNAL_GUARD__
#define __NotMyChildException_EXTERNAL_GUARD__
#include <NotMyChildException.hh>
#endif
#ifndef __NodeIterator_EXTERNAL_GUARD__
#define __NodeIterator_EXTERNAL_GUARD__
#include <NodeIterator.hh>
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

#ifndef __ProcessingInstruction__
#define __ProcessingInstruction__
class   ProcessingInstruction;
typedef ProcessingInstruction* ProcessingInstruction_ptr;
typedef ProcessingInstruction_ptr ProcessingInstructionRef;

class _wrap_home_ProcessingInstruction;

class ProcessingInstruction_Helper {
  public:
  static ProcessingInstruction_ptr _nil();
  static CORBA::Boolean is_nil(ProcessingInstruction_ptr p);
  static void release(ProcessingInstruction_ptr p);
  static void duplicate(ProcessingInstruction_ptr p);
  static size_t NP_alignedSize(ProcessingInstruction_ptr obj,size_t initialoffset);
  static void marshalObjRef(ProcessingInstruction_ptr obj,NetBufferedStream &s);
  static ProcessingInstruction_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(ProcessingInstruction_ptr obj,MemBufferedStream &s);
  static ProcessingInstruction_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<ProcessingInstruction,ProcessingInstruction_Helper> ProcessingInstruction_var;

#endif
#define ProcessingInstruction_IntfRepoID "IDL:ProcessingInstruction:1.0"

class ProcessingInstruction :  public virtual Node {
public:

  virtual wstring * name ()  = 0;
  virtual void name (const wstring & _value) = 0;
  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  static ProcessingInstruction_ptr _duplicate(ProcessingInstruction_ptr);
  static ProcessingInstruction_ptr _narrow(CORBA::Object_ptr);
  static ProcessingInstruction_ptr _nil();

  static inline size_t NP_alignedSize(ProcessingInstruction_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,ProcessingInstruction_IntfRepoID,30,initialoffset);
  }

  static inline void marshalObjRef(ProcessingInstruction_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,ProcessingInstruction_IntfRepoID,30,s);
  }

  static inline ProcessingInstruction_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(ProcessingInstruction_IntfRepoID,s);
    ProcessingInstruction_ptr _result = ProcessingInstruction::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(ProcessingInstruction_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,ProcessingInstruction_IntfRepoID,30,s);
  }

  static inline ProcessingInstruction_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(ProcessingInstruction_IntfRepoID,s);
    ProcessingInstruction_ptr _result = ProcessingInstruction::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  ProcessingInstruction() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(ProcessingInstruction_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~ProcessingInstruction() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  ProcessingInstruction(const ProcessingInstruction&);
  ProcessingInstruction &operator=(const ProcessingInstruction&);
  friend class _wrap_home_ProcessingInstruction;
};

class _sk_ProcessingInstruction :  public virtual _sk_Node, public virtual ProcessingInstruction {
public:

  _sk_ProcessingInstruction() {}
  _sk_ProcessingInstruction(const omniORB::objectKey& k);
  virtual ~_sk_ProcessingInstruction() {}
  ProcessingInstruction_ptr _this() { return ProcessingInstruction::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual wstring * name ()  = 0;
  virtual void name (const wstring & _value) = 0;
  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return ProcessingInstruction::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_ProcessingInstruction (const _sk_ProcessingInstruction&);
  _sk_ProcessingInstruction &operator=(const _sk_ProcessingInstruction&);
};

class _proxy_ProcessingInstruction :  public virtual _proxy_Node, public virtual ProcessingInstruction {
public:

  _proxy_ProcessingInstruction (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(ProcessingInstruction_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_ProcessingInstruction() {}
  virtual wstring * name () ;
  virtual void name (const wstring & _value);
  virtual wstring * data () ;
  virtual void data (const wstring & _value);

protected:

  _proxy_ProcessingInstruction () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return ProcessingInstruction::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_ProcessingInstruction (const _proxy_ProcessingInstruction&);
  _proxy_ProcessingInstruction &operator=(const _proxy_ProcessingInstruction&);
};

class _nil_ProcessingInstruction :  public virtual _nil_Node, public virtual ProcessingInstruction {
public:
  _nil_ProcessingInstruction() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_ProcessingInstruction() {}
  wstring * name ()  {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void name (const wstring & _value) {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
  }

  wstring * data ()  {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void data (const wstring & _value) {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return ProcessingInstruction::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_ProcessingInstruction :  public virtual _lc_sk_Node, public virtual ProcessingInstruction {
public:

  _lc_sk_ProcessingInstruction() {}
  _lc_sk_ProcessingInstruction(const omniORB::objectKey& k);
  virtual ~_lc_sk_ProcessingInstruction() {}
  ProcessingInstruction_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual wstring * name ()  = 0;
  virtual void name (const wstring & _value) = 0;
  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return ProcessingInstruction::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_ProcessingInstruction (const _lc_sk_ProcessingInstruction&);
  _lc_sk_ProcessingInstruction &operator=(const _lc_sk_ProcessingInstruction&);
  ProcessingInstruction_var _home_ProcessingInstruction;
};

class _dead_ProcessingInstruction :  public virtual _dead_Node, public virtual ProcessingInstruction {
public:
  _dead_ProcessingInstruction() { }
  virtual ~_dead_ProcessingInstruction() {}
  wstring * name ()  {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void name (const wstring & _value) {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
  }

  wstring * data ()  {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void data (const wstring & _value) {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return ProcessingInstruction::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_ProcessingInstruction :  public virtual _wrap_home_Node, public virtual ProcessingInstruction {
private:
  ProcessingInstruction_ptr _actual_ProcessingInstruction;

protected:
  void _set_actual(ProcessingInstruction_ptr p);
  void _release_actual();
  _wrap_home_ProcessingInstruction() { }
public:
  _wrap_home_ProcessingInstruction(_lc_sk_ProcessingInstruction *sk);
  ~_wrap_home_ProcessingInstruction();

  void _move(CORBA::Object_ptr to);
  void _remove();

  wstring * name () {
    return _actual_ProcessingInstruction->name();
  }

  void name (const wstring & _value){
    _actual_ProcessingInstruction->name(_value);
  }

  wstring * data () {
    return _actual_ProcessingInstruction->data();
  }

  void data (const wstring & _value){
    _actual_ProcessingInstruction->data(_value);
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_ProcessingInstruction;
class _lc_proxy_ProcessingInstruction :  public virtual _lc_proxy_Node, public virtual ProcessingInstruction {
public:

  _lc_proxy_ProcessingInstruction (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(ProcessingInstruction_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_ProcessingInstruction() {}
  void _set_wrap_ProcessingInstruction(_wrap_proxy_ProcessingInstruction *wrap) {
    _wrap_ProcessingInstruction = wrap;
    _lc_proxy_Node::_set_wrap_Node((_wrap_proxy_Node *)wrap);
  }

  virtual wstring * name () ;
  virtual void name (const wstring & _value);
  virtual wstring * data () ;
  virtual void data (const wstring & _value);

protected:

  _lc_proxy_ProcessingInstruction () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return ProcessingInstruction::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_ProcessingInstruction *_get_wrap_ProcessingInstruction() {
    return _wrap_ProcessingInstruction;
  }
  _wrap_proxy_ProcessingInstruction *_wrap_ProcessingInstruction;
  _lc_proxy_ProcessingInstruction (const _lc_proxy_ProcessingInstruction&);
  _lc_proxy_ProcessingInstruction &operator=(const _lc_proxy_ProcessingInstruction&);
  friend class _wrap_proxy_ProcessingInstruction;
};

class _wrap_proxy_ProcessingInstruction :  public virtual _wrap_proxy_Node, public virtual ProcessingInstruction {
private:
  _lc_proxy_ProcessingInstruction *_orig_ProcessingInstruction;
  ProcessingInstruction_var _actual_ProcessingInstruction;

public:

  _wrap_proxy_ProcessingInstruction (_lc_proxy_ProcessingInstruction *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_ProcessingInstruction();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  wstring * name () {
    return _actual_ProcessingInstruction->name();
  }

  void name (const wstring & _value){
    _actual_ProcessingInstruction->name(_value);
  }

  wstring * data () {
    return _actual_ProcessingInstruction->data();
  }

  void data (const wstring & _value){
    _actual_ProcessingInstruction->data(_value);
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_ProcessingInstruction *proxy);
  _wrap_proxy_ProcessingInstruction() {}
};

// *** End of LifeCycle stuff
class ProcessingInstruction_proxyObjectFactory : public proxyObjectFactory {
public:
  ProcessingInstruction_proxyObjectFactory () {}
  virtual ~ProcessingInstruction_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static ProcessingInstruction_ptr _nil() {
    if (!__nil_ProcessingInstruction) {
      __nil_ProcessingInstruction = new _nil_ProcessingInstruction;
    }
    return __nil_ProcessingInstruction;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static ProcessingInstruction_ptr __nil_ProcessingInstruction;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_ProcessingInstruction;


void operator<<=(CORBA::Any& _a, ProcessingInstruction_ptr _s);
void operator<<=(CORBA::Any& _a, ProcessingInstruction_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, ProcessingInstruction_ptr& _s);


#undef _LC_attr

#endif // __ProcessingInstruction_hh__
