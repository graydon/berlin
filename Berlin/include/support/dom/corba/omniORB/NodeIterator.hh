#ifndef __NodeIterator_hh__
#define __NodeIterator_hh__

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

#ifdef _LC_attr
#error "A local CPP macro _LC_attr has already been defined."
#else
#ifdef  USE_stub_in_nt_dll
#define _LC_attr _OMNIORB_NTDLL_IMPORT
#else
#define _LC_attr
#endif
#endif

#ifndef __Node__
#define __Node__
class   Node;
typedef Node* Node_ptr;
typedef Node_ptr NodeRef;
class _proxy_Node;
class _sk_Node;
class _nil_Node;
class _lc_proxy_Node;
class _lc_sk_Node;
class _dead_Node;
class _wrap_home_Node;
class _wrap_proxy_Node;

class Node_Helper {
  public:
  static Node_ptr _nil();
  static CORBA::Boolean is_nil(Node_ptr p);
  static void release(Node_ptr p);
  static void duplicate(Node_ptr p);
  static size_t NP_alignedSize(Node_ptr obj,size_t initialoffset);
  static void marshalObjRef(Node_ptr obj,NetBufferedStream &s);
  static Node_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Node_ptr obj,MemBufferedStream &s);
  static Node_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Node,Node_Helper> Node_var;

#endif
#ifndef __NodeIterator__
#define __NodeIterator__
class   NodeIterator;
typedef NodeIterator* NodeIterator_ptr;
typedef NodeIterator_ptr NodeIteratorRef;

class _wrap_home_NodeIterator;

class NodeIterator_Helper {
  public:
  static NodeIterator_ptr _nil();
  static CORBA::Boolean is_nil(NodeIterator_ptr p);
  static void release(NodeIterator_ptr p);
  static void duplicate(NodeIterator_ptr p);
  static size_t NP_alignedSize(NodeIterator_ptr obj,size_t initialoffset);
  static void marshalObjRef(NodeIterator_ptr obj,NetBufferedStream &s);
  static NodeIterator_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(NodeIterator_ptr obj,MemBufferedStream &s);
  static NodeIterator_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<NodeIterator,NodeIterator_Helper> NodeIterator_var;

#endif
#define NodeIterator_IntfRepoID "IDL:NodeIterator:1.0"

class NodeIterator : public virtual omniObject, public virtual CORBA::Object {
public:

  virtual CORBA::ULong  getLength (  ) = 0;
  virtual CORBA::ULong  getCurrentPos (  ) = 0;
  virtual CORBA::Boolean  atFirst (  ) = 0;
  virtual CORBA::Boolean  atLast (  ) = 0;
  virtual Node_ptr  toNextNode (  ) = 0;
  virtual Node_ptr  toPrevNode (  ) = 0;
  virtual Node_ptr  toFirstNode (  ) = 0;
  virtual Node_ptr  toLastNode (  ) = 0;
  virtual Node_ptr  moveTo ( CORBA::Long  n ) = 0;
  static NodeIterator_ptr _duplicate(NodeIterator_ptr);
  static NodeIterator_ptr _narrow(CORBA::Object_ptr);
  static NodeIterator_ptr _nil();

  static inline size_t NP_alignedSize(NodeIterator_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,NodeIterator_IntfRepoID,21,initialoffset);
  }

  static inline void marshalObjRef(NodeIterator_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,NodeIterator_IntfRepoID,21,s);
  }

  static inline NodeIterator_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(NodeIterator_IntfRepoID,s);
    NodeIterator_ptr _result = NodeIterator::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(NodeIterator_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,NodeIterator_IntfRepoID,21,s);
  }

  static inline NodeIterator_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(NodeIterator_IntfRepoID,s);
    NodeIterator_ptr _result = NodeIterator::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  NodeIterator() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(NodeIterator_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~NodeIterator() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  NodeIterator(const NodeIterator&);
  NodeIterator &operator=(const NodeIterator&);
  friend class _wrap_home_NodeIterator;
};

class _sk_NodeIterator :  public virtual NodeIterator {
public:

  _sk_NodeIterator() {}
  _sk_NodeIterator(const omniORB::objectKey& k);
  virtual ~_sk_NodeIterator() {}
  NodeIterator_ptr _this() { return NodeIterator::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual CORBA::ULong  getLength (  ) = 0;
  virtual CORBA::ULong  getCurrentPos (  ) = 0;
  virtual CORBA::Boolean  atFirst (  ) = 0;
  virtual CORBA::Boolean  atLast (  ) = 0;
  virtual Node_ptr  toNextNode (  ) = 0;
  virtual Node_ptr  toPrevNode (  ) = 0;
  virtual Node_ptr  toFirstNode (  ) = 0;
  virtual Node_ptr  toLastNode (  ) = 0;
  virtual Node_ptr  moveTo ( CORBA::Long  n ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return NodeIterator::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_NodeIterator (const _sk_NodeIterator&);
  _sk_NodeIterator &operator=(const _sk_NodeIterator&);
};

class _proxy_NodeIterator :  public virtual NodeIterator {
public:

  _proxy_NodeIterator (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(NodeIterator_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_NodeIterator() {}
  virtual CORBA::ULong  getLength (  );
  virtual CORBA::ULong  getCurrentPos (  );
  virtual CORBA::Boolean  atFirst (  );
  virtual CORBA::Boolean  atLast (  );
  virtual Node_ptr  toNextNode (  );
  virtual Node_ptr  toPrevNode (  );
  virtual Node_ptr  toFirstNode (  );
  virtual Node_ptr  toLastNode (  );
  virtual Node_ptr  moveTo ( CORBA::Long  n );

protected:

  _proxy_NodeIterator () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return NodeIterator::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_NodeIterator (const _proxy_NodeIterator&);
  _proxy_NodeIterator &operator=(const _proxy_NodeIterator&);
};

class _nil_NodeIterator : public virtual NodeIterator {
public:
  _nil_NodeIterator() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_NodeIterator() {}
  CORBA::ULong  getLength (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::ULong _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::ULong  getCurrentPos (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::ULong _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  atFirst (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  atLast (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toNextNode (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toPrevNode (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toFirstNode (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toLastNode (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  moveTo ( CORBA::Long  n ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return NodeIterator::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_NodeIterator :  public virtual omniLC::_lc_sk, public virtual NodeIterator {
public:

  _lc_sk_NodeIterator() {}
  _lc_sk_NodeIterator(const omniORB::objectKey& k);
  virtual ~_lc_sk_NodeIterator() {}
  NodeIterator_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual CORBA::ULong  getLength (  ) = 0;
  virtual CORBA::ULong  getCurrentPos (  ) = 0;
  virtual CORBA::Boolean  atFirst (  ) = 0;
  virtual CORBA::Boolean  atLast (  ) = 0;
  virtual Node_ptr  toNextNode (  ) = 0;
  virtual Node_ptr  toPrevNode (  ) = 0;
  virtual Node_ptr  toFirstNode (  ) = 0;
  virtual Node_ptr  toLastNode (  ) = 0;
  virtual Node_ptr  moveTo ( CORBA::Long  n ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return NodeIterator::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_NodeIterator (const _lc_sk_NodeIterator&);
  _lc_sk_NodeIterator &operator=(const _lc_sk_NodeIterator&);
  NodeIterator_var _home_NodeIterator;
};

class _dead_NodeIterator : public virtual NodeIterator {
public:
  _dead_NodeIterator() { }
  virtual ~_dead_NodeIterator() {}
  CORBA::ULong  getLength (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::ULong _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::ULong  getCurrentPos (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::ULong _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  atFirst (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  atLast (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toNextNode (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toPrevNode (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toFirstNode (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  toLastNode (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  moveTo ( CORBA::Long  n ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
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
    return NodeIterator::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_NodeIterator :  public virtual omniLC::_wrap_home, public virtual NodeIterator {
private:
  NodeIterator_ptr _actual_NodeIterator;

protected:
  void _set_actual(NodeIterator_ptr p);
  void _release_actual();
  _wrap_home_NodeIterator() { }
public:
  _wrap_home_NodeIterator(_lc_sk_NodeIterator *sk);
  ~_wrap_home_NodeIterator();

  void _move(CORBA::Object_ptr to);
  void _remove();

  CORBA::ULong  getLength (  ){
    return _actual_NodeIterator->getLength (  );
  }

  CORBA::ULong  getCurrentPos (  ){
    return _actual_NodeIterator->getCurrentPos (  );
  }

  CORBA::Boolean  atFirst (  ){
    return _actual_NodeIterator->atFirst (  );
  }

  CORBA::Boolean  atLast (  ){
    return _actual_NodeIterator->atLast (  );
  }

  Node_ptr  toNextNode (  ){
    return _actual_NodeIterator->toNextNode (  );
  }

  Node_ptr  toPrevNode (  ){
    return _actual_NodeIterator->toPrevNode (  );
  }

  Node_ptr  toFirstNode (  ){
    return _actual_NodeIterator->toFirstNode (  );
  }

  Node_ptr  toLastNode (  ){
    return _actual_NodeIterator->toLastNode (  );
  }

  Node_ptr  moveTo ( CORBA::Long  n ){
    return _actual_NodeIterator->moveTo ( n );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_NodeIterator;
class _lc_proxy_NodeIterator :  public virtual NodeIterator {
public:

  _lc_proxy_NodeIterator (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(NodeIterator_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_NodeIterator() {}
  void _set_wrap_NodeIterator(_wrap_proxy_NodeIterator *wrap) {
    _wrap_NodeIterator = wrap;
  }

  virtual CORBA::ULong  getLength (  );
  virtual CORBA::ULong  getCurrentPos (  );
  virtual CORBA::Boolean  atFirst (  );
  virtual CORBA::Boolean  atLast (  );
  virtual Node_ptr  toNextNode (  );
  virtual Node_ptr  toPrevNode (  );
  virtual Node_ptr  toFirstNode (  );
  virtual Node_ptr  toLastNode (  );
  virtual Node_ptr  moveTo ( CORBA::Long  n );

protected:

  _lc_proxy_NodeIterator () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return NodeIterator::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_NodeIterator *_get_wrap_NodeIterator() {
    return _wrap_NodeIterator;
  }
  _wrap_proxy_NodeIterator *_wrap_NodeIterator;
  _lc_proxy_NodeIterator (const _lc_proxy_NodeIterator&);
  _lc_proxy_NodeIterator &operator=(const _lc_proxy_NodeIterator&);
  friend class _wrap_proxy_NodeIterator;
};

class _wrap_proxy_NodeIterator :  public virtual omniLC::_wrap_proxy, public virtual NodeIterator {
private:
  _lc_proxy_NodeIterator *_orig_NodeIterator;
  NodeIterator_var _actual_NodeIterator;

public:

  _wrap_proxy_NodeIterator (_lc_proxy_NodeIterator *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_NodeIterator();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  CORBA::ULong  getLength (  ){
    return _actual_NodeIterator->getLength (  );
  }

  CORBA::ULong  getCurrentPos (  ){
    return _actual_NodeIterator->getCurrentPos (  );
  }

  CORBA::Boolean  atFirst (  ){
    return _actual_NodeIterator->atFirst (  );
  }

  CORBA::Boolean  atLast (  ){
    return _actual_NodeIterator->atLast (  );
  }

  Node_ptr  toNextNode (  ){
    return _actual_NodeIterator->toNextNode (  );
  }

  Node_ptr  toPrevNode (  ){
    return _actual_NodeIterator->toPrevNode (  );
  }

  Node_ptr  toFirstNode (  ){
    return _actual_NodeIterator->toFirstNode (  );
  }

  Node_ptr  toLastNode (  ){
    return _actual_NodeIterator->toLastNode (  );
  }

  Node_ptr  moveTo ( CORBA::Long  n ){
    return _actual_NodeIterator->moveTo ( n );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_NodeIterator *proxy);
  _wrap_proxy_NodeIterator() {}
};

// *** End of LifeCycle stuff
class NodeIterator_proxyObjectFactory : public proxyObjectFactory {
public:
  NodeIterator_proxyObjectFactory () {}
  virtual ~NodeIterator_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static NodeIterator_ptr _nil() {
    if (!__nil_NodeIterator) {
      __nil_NodeIterator = new _nil_NodeIterator;
    }
    return __nil_NodeIterator;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static NodeIterator_ptr __nil_NodeIterator;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_NodeIterator;


void operator<<=(CORBA::Any& _a, NodeIterator_ptr _s);
void operator<<=(CORBA::Any& _a, NodeIterator_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, NodeIterator_ptr& _s);


#undef _LC_attr

#endif // __NodeIterator_hh__
