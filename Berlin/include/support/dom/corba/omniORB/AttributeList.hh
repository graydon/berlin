#ifndef __AttributeList_hh__
#define __AttributeList_hh__

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

#ifndef __Attribute_EXTERNAL_GUARD__
#define __Attribute_EXTERNAL_GUARD__
#include <Attribute.hh>
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

#ifndef __AttributeList__
#define __AttributeList__
class   AttributeList;
typedef AttributeList* AttributeList_ptr;
typedef AttributeList_ptr AttributeListRef;

class _wrap_home_AttributeList;

class AttributeList_Helper {
  public:
  static AttributeList_ptr _nil();
  static CORBA::Boolean is_nil(AttributeList_ptr p);
  static void release(AttributeList_ptr p);
  static void duplicate(AttributeList_ptr p);
  static size_t NP_alignedSize(AttributeList_ptr obj,size_t initialoffset);
  static void marshalObjRef(AttributeList_ptr obj,NetBufferedStream &s);
  static AttributeList_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(AttributeList_ptr obj,MemBufferedStream &s);
  static AttributeList_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<AttributeList,AttributeList_Helper> AttributeList_var;

#endif
#define AttributeList_IntfRepoID "IDL:AttributeList:1.0"

class AttributeList : public virtual omniObject, public virtual CORBA::Object {
public:

  virtual Attrib_ptr  getAttribute ( const wstring & attrName ) = 0;
  virtual Attrib_ptr  setAttribute ( Attrib_ptr  attr ) = 0;
  virtual Attrib_ptr  remove ( const wstring & attrName ) = 0;
  virtual Attrib_ptr  item ( CORBA::ULong  index ) = 0;
  virtual CORBA::ULong  getLength (  ) = 0;
  static AttributeList_ptr _duplicate(AttributeList_ptr);
  static AttributeList_ptr _narrow(CORBA::Object_ptr);
  static AttributeList_ptr _nil();

  static inline size_t NP_alignedSize(AttributeList_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,AttributeList_IntfRepoID,22,initialoffset);
  }

  static inline void marshalObjRef(AttributeList_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,AttributeList_IntfRepoID,22,s);
  }

  static inline AttributeList_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(AttributeList_IntfRepoID,s);
    AttributeList_ptr _result = AttributeList::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(AttributeList_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,AttributeList_IntfRepoID,22,s);
  }

  static inline AttributeList_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(AttributeList_IntfRepoID,s);
    AttributeList_ptr _result = AttributeList::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  AttributeList() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(AttributeList_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~AttributeList() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  AttributeList(const AttributeList&);
  AttributeList &operator=(const AttributeList&);
  friend class _wrap_home_AttributeList;
};

class _sk_AttributeList :  public virtual AttributeList {
public:

  _sk_AttributeList() {}
  _sk_AttributeList(const omniORB::objectKey& k);
  virtual ~_sk_AttributeList() {}
  AttributeList_ptr _this() { return AttributeList::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual Attrib_ptr  getAttribute ( const wstring & attrName ) = 0;
  virtual Attrib_ptr  setAttribute ( Attrib_ptr  attr ) = 0;
  virtual Attrib_ptr  remove ( const wstring & attrName ) = 0;
  virtual Attrib_ptr  item ( CORBA::ULong  index ) = 0;
  virtual CORBA::ULong  getLength (  ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return AttributeList::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_AttributeList (const _sk_AttributeList&);
  _sk_AttributeList &operator=(const _sk_AttributeList&);
};

class _proxy_AttributeList :  public virtual AttributeList {
public:

  _proxy_AttributeList (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(AttributeList_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_AttributeList() {}
  virtual Attrib_ptr  getAttribute ( const wstring & attrName );
  virtual Attrib_ptr  setAttribute ( Attrib_ptr  attr );
  virtual Attrib_ptr  remove ( const wstring & attrName );
  virtual Attrib_ptr  item ( CORBA::ULong  index );
  virtual CORBA::ULong  getLength (  );

protected:

  _proxy_AttributeList () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return AttributeList::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_AttributeList (const _proxy_AttributeList&);
  _proxy_AttributeList &operator=(const _proxy_AttributeList&);
};

class _nil_AttributeList : public virtual AttributeList {
public:
  _nil_AttributeList() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_AttributeList() {}
  Attrib_ptr  getAttribute ( const wstring & attrName ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  setAttribute ( Attrib_ptr  attr ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  remove ( const wstring & attrName ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  item ( CORBA::ULong  index ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::ULong  getLength (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::ULong _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return AttributeList::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_AttributeList :  public virtual omniLC::_lc_sk, public virtual AttributeList {
public:

  _lc_sk_AttributeList() {}
  _lc_sk_AttributeList(const omniORB::objectKey& k);
  virtual ~_lc_sk_AttributeList() {}
  AttributeList_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual Attrib_ptr  getAttribute ( const wstring & attrName ) = 0;
  virtual Attrib_ptr  setAttribute ( Attrib_ptr  attr ) = 0;
  virtual Attrib_ptr  remove ( const wstring & attrName ) = 0;
  virtual Attrib_ptr  item ( CORBA::ULong  index ) = 0;
  virtual CORBA::ULong  getLength (  ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return AttributeList::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_AttributeList (const _lc_sk_AttributeList&);
  _lc_sk_AttributeList &operator=(const _lc_sk_AttributeList&);
  AttributeList_var _home_AttributeList;
};

class _dead_AttributeList : public virtual AttributeList {
public:
  _dead_AttributeList() { }
  virtual ~_dead_AttributeList() {}
  Attrib_ptr  getAttribute ( const wstring & attrName ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  setAttribute ( Attrib_ptr  attr ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  remove ( const wstring & attrName ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  item ( CORBA::ULong  index ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::ULong  getLength (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::ULong _0RL_result = 0;
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
    return AttributeList::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_AttributeList :  public virtual omniLC::_wrap_home, public virtual AttributeList {
private:
  AttributeList_ptr _actual_AttributeList;

protected:
  void _set_actual(AttributeList_ptr p);
  void _release_actual();
  _wrap_home_AttributeList() { }
public:
  _wrap_home_AttributeList(_lc_sk_AttributeList *sk);
  ~_wrap_home_AttributeList();

  void _move(CORBA::Object_ptr to);
  void _remove();

  Attrib_ptr  getAttribute ( const wstring & attrName ){
    return _actual_AttributeList->getAttribute ( attrName );
  }

  Attrib_ptr  setAttribute ( Attrib_ptr  attr ){
    return _actual_AttributeList->setAttribute ( attr );
  }

  Attrib_ptr  remove ( const wstring & attrName ){
    return _actual_AttributeList->remove ( attrName );
  }

  Attrib_ptr  item ( CORBA::ULong  index ){
    return _actual_AttributeList->item ( index );
  }

  CORBA::ULong  getLength (  ){
    return _actual_AttributeList->getLength (  );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_AttributeList;
class _lc_proxy_AttributeList :  public virtual AttributeList {
public:

  _lc_proxy_AttributeList (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(AttributeList_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_AttributeList() {}
  void _set_wrap_AttributeList(_wrap_proxy_AttributeList *wrap) {
    _wrap_AttributeList = wrap;
  }

  virtual Attrib_ptr  getAttribute ( const wstring & attrName );
  virtual Attrib_ptr  setAttribute ( Attrib_ptr  attr );
  virtual Attrib_ptr  remove ( const wstring & attrName );
  virtual Attrib_ptr  item ( CORBA::ULong  index );
  virtual CORBA::ULong  getLength (  );

protected:

  _lc_proxy_AttributeList () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return AttributeList::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_AttributeList *_get_wrap_AttributeList() {
    return _wrap_AttributeList;
  }
  _wrap_proxy_AttributeList *_wrap_AttributeList;
  _lc_proxy_AttributeList (const _lc_proxy_AttributeList&);
  _lc_proxy_AttributeList &operator=(const _lc_proxy_AttributeList&);
  friend class _wrap_proxy_AttributeList;
};

class _wrap_proxy_AttributeList :  public virtual omniLC::_wrap_proxy, public virtual AttributeList {
private:
  _lc_proxy_AttributeList *_orig_AttributeList;
  AttributeList_var _actual_AttributeList;

public:

  _wrap_proxy_AttributeList (_lc_proxy_AttributeList *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_AttributeList();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  Attrib_ptr  getAttribute ( const wstring & attrName ){
    return _actual_AttributeList->getAttribute ( attrName );
  }

  Attrib_ptr  setAttribute ( Attrib_ptr  attr ){
    return _actual_AttributeList->setAttribute ( attr );
  }

  Attrib_ptr  remove ( const wstring & attrName ){
    return _actual_AttributeList->remove ( attrName );
  }

  Attrib_ptr  item ( CORBA::ULong  index ){
    return _actual_AttributeList->item ( index );
  }

  CORBA::ULong  getLength (  ){
    return _actual_AttributeList->getLength (  );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_AttributeList *proxy);
  _wrap_proxy_AttributeList() {}
};

// *** End of LifeCycle stuff
class AttributeList_proxyObjectFactory : public proxyObjectFactory {
public:
  AttributeList_proxyObjectFactory () {}
  virtual ~AttributeList_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static AttributeList_ptr _nil() {
    if (!__nil_AttributeList) {
      __nil_AttributeList = new _nil_AttributeList;
    }
    return __nil_AttributeList;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static AttributeList_ptr __nil_AttributeList;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_AttributeList;


void operator<<=(CORBA::Any& _a, AttributeList_ptr _s);
void operator<<=(CORBA::Any& _a, AttributeList_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, AttributeList_ptr& _s);


#undef _LC_attr

#endif // __AttributeList_hh__
