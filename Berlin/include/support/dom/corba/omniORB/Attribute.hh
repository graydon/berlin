#ifndef __Attribute_hh__
#define __Attribute_hh__

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

#ifndef __domDefs_EXTERNAL_GUARD__
#define __domDefs_EXTERNAL_GUARD__
#include <domDefs.hh>
#endif
#ifndef __Node_EXTERNAL_GUARD__
#define __Node_EXTERNAL_GUARD__
#include <Node.hh>
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

#ifndef __Attrib__
#define __Attrib__
class   Attrib;
typedef Attrib* Attrib_ptr;
typedef Attrib_ptr AttribRef;

class _wrap_home_Attrib;

class Attrib_Helper {
  public:
  static Attrib_ptr _nil();
  static CORBA::Boolean is_nil(Attrib_ptr p);
  static void release(Attrib_ptr p);
  static void duplicate(Attrib_ptr p);
  static size_t NP_alignedSize(Attrib_ptr obj,size_t initialoffset);
  static void marshalObjRef(Attrib_ptr obj,NetBufferedStream &s);
  static Attrib_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Attrib_ptr obj,MemBufferedStream &s);
  static Attrib_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Attrib,Attrib_Helper> Attrib_var;

#endif
#define Attrib_IntfRepoID "IDL:Attrib:1.0"

class Attrib :  public virtual Node {
public:

  virtual wstring * getName (  ) = 0;
  virtual wstring * getValue (  ) = 0;
  virtual CORBA::Boolean  specified ()  = 0;
  virtual void specified (CORBA::Boolean  _value) = 0;
  virtual wstring * toString (  ) = 0;
  static Attrib_ptr _duplicate(Attrib_ptr);
  static Attrib_ptr _narrow(CORBA::Object_ptr);
  static Attrib_ptr _nil();

  static inline size_t NP_alignedSize(Attrib_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Attrib_IntfRepoID,15,initialoffset);
  }

  static inline void marshalObjRef(Attrib_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Attrib_IntfRepoID,15,s);
  }

  static inline Attrib_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Attrib_IntfRepoID,s);
    Attrib_ptr _result = Attrib::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Attrib_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Attrib_IntfRepoID,15,s);
  }

  static inline Attrib_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Attrib_IntfRepoID,s);
    Attrib_ptr _result = Attrib::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Attrib() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Attrib_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Attrib() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Attrib(const Attrib&);
  Attrib &operator=(const Attrib&);
  friend class _wrap_home_Attrib;
};

class _sk_Attrib :  public virtual _sk_Node, public virtual Attrib {
public:

  _sk_Attrib() {}
  _sk_Attrib(const omniORB::objectKey& k);
  virtual ~_sk_Attrib() {}
  Attrib_ptr _this() { return Attrib::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual wstring * getName (  ) = 0;
  virtual wstring * getValue (  ) = 0;
  virtual CORBA::Boolean  specified ()  = 0;
  virtual void specified (CORBA::Boolean  _value) = 0;
  virtual wstring * toString (  ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Attrib::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Attrib (const _sk_Attrib&);
  _sk_Attrib &operator=(const _sk_Attrib&);
};

class _proxy_Attrib :  public virtual _proxy_Node, public virtual Attrib {
public:

  _proxy_Attrib (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Attrib_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Attrib() {}
  virtual wstring * getName (  );
  virtual wstring * getValue (  );
  virtual CORBA::Boolean  specified () ;
  virtual void specified (CORBA::Boolean  _value);
  virtual wstring * toString (  );

protected:

  _proxy_Attrib () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Attrib::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Attrib (const _proxy_Attrib&);
  _proxy_Attrib &operator=(const _proxy_Attrib&);
};

class _nil_Attrib :  public virtual _nil_Node, public virtual Attrib {
public:
  _nil_Attrib() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Attrib() {}
  wstring * getName (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  wstring * getValue (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  specified ()  {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void specified (CORBA::Boolean  _value) {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
  }

  wstring * toString (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Attrib::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Attrib :  public virtual _lc_sk_Node, public virtual Attrib {
public:

  _lc_sk_Attrib() {}
  _lc_sk_Attrib(const omniORB::objectKey& k);
  virtual ~_lc_sk_Attrib() {}
  Attrib_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual wstring * getName (  ) = 0;
  virtual wstring * getValue (  ) = 0;
  virtual CORBA::Boolean  specified ()  = 0;
  virtual void specified (CORBA::Boolean  _value) = 0;
  virtual wstring * toString (  ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Attrib::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Attrib (const _lc_sk_Attrib&);
  _lc_sk_Attrib &operator=(const _lc_sk_Attrib&);
  Attrib_var _home_Attrib;
};

class _dead_Attrib :  public virtual _dead_Node, public virtual Attrib {
public:
  _dead_Attrib() { }
  virtual ~_dead_Attrib() {}
  wstring * getName (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  wstring * getValue (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  specified ()  {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void specified (CORBA::Boolean  _value) {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
  }

  wstring * toString (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
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
    return Attrib::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Attrib :  public virtual _wrap_home_Node, public virtual Attrib {
private:
  Attrib_ptr _actual_Attrib;

protected:
  void _set_actual(Attrib_ptr p);
  void _release_actual();
  _wrap_home_Attrib() { }
public:
  _wrap_home_Attrib(_lc_sk_Attrib *sk);
  ~_wrap_home_Attrib();

  void _move(CORBA::Object_ptr to);
  void _remove();

  wstring * getName (  ){
    return _actual_Attrib->getName (  );
  }

  wstring * getValue (  ){
    return _actual_Attrib->getValue (  );
  }

  CORBA::Boolean  specified () {
    return _actual_Attrib->specified();
  }

  void specified (CORBA::Boolean  _value){
    _actual_Attrib->specified(_value);
  }

  wstring * toString (  ){
    return _actual_Attrib->toString (  );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Attrib;
class _lc_proxy_Attrib :  public virtual _lc_proxy_Node, public virtual Attrib {
public:

  _lc_proxy_Attrib (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Attrib_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Attrib() {}
  void _set_wrap_Attrib(_wrap_proxy_Attrib *wrap) {
    _wrap_Attrib = wrap;
    _lc_proxy_Node::_set_wrap_Node((_wrap_proxy_Node *)wrap);
  }

  virtual wstring * getName (  );
  virtual wstring * getValue (  );
  virtual CORBA::Boolean  specified () ;
  virtual void specified (CORBA::Boolean  _value);
  virtual wstring * toString (  );

protected:

  _lc_proxy_Attrib () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Attrib::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Attrib *_get_wrap_Attrib() {
    return _wrap_Attrib;
  }
  _wrap_proxy_Attrib *_wrap_Attrib;
  _lc_proxy_Attrib (const _lc_proxy_Attrib&);
  _lc_proxy_Attrib &operator=(const _lc_proxy_Attrib&);
  friend class _wrap_proxy_Attrib;
};

class _wrap_proxy_Attrib :  public virtual _wrap_proxy_Node, public virtual Attrib {
private:
  _lc_proxy_Attrib *_orig_Attrib;
  Attrib_var _actual_Attrib;

public:

  _wrap_proxy_Attrib (_lc_proxy_Attrib *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Attrib();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  wstring * getName (  ){
    return _actual_Attrib->getName (  );
  }

  wstring * getValue (  ){
    return _actual_Attrib->getValue (  );
  }

  CORBA::Boolean  specified () {
    return _actual_Attrib->specified();
  }

  void specified (CORBA::Boolean  _value){
    _actual_Attrib->specified(_value);
  }

  wstring * toString (  ){
    return _actual_Attrib->toString (  );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Attrib *proxy);
  _wrap_proxy_Attrib() {}
};

// *** End of LifeCycle stuff
class Attrib_proxyObjectFactory : public proxyObjectFactory {
public:
  Attrib_proxyObjectFactory () {}
  virtual ~Attrib_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Attrib_ptr _nil() {
    if (!__nil_Attrib) {
      __nil_Attrib = new _nil_Attrib;
    }
    return __nil_Attrib;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Attrib_ptr __nil_Attrib;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Attrib;


void operator<<=(CORBA::Any& _a, Attrib_ptr _s);
void operator<<=(CORBA::Any& _a, Attrib_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Attrib_ptr& _s);


#undef _LC_attr

#endif // __Attribute_hh__
