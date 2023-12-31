#ifndef __Node_hh__
#define __Node_hh__

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

#ifndef __NodeIterator__
#define __NodeIterator__
class   NodeIterator;
typedef NodeIterator* NodeIterator_ptr;
typedef NodeIterator_ptr NodeIteratorRef;
class _proxy_NodeIterator;
class _sk_NodeIterator;
class _nil_NodeIterator;
class _lc_proxy_NodeIterator;
class _lc_sk_NodeIterator;
class _dead_NodeIterator;
class _wrap_home_NodeIterator;
class _wrap_proxy_NodeIterator;

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
#ifndef __Node__
#define __Node__
class   Node;
typedef Node* Node_ptr;
typedef Node_ptr NodeRef;

class _wrap_home_Node;

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
#define Node_IntfRepoID "IDL:Node:1.0"

class Node : public virtual omniObject, public virtual CORBA::Object {
public:

  enum NodeType { DOCUMENT, ELEMENT, ATTRIBUTE, PI, COMMENT, TEXT };

  static _LC_attr const CORBA::TypeCode_ptr _tc_NodeType;

  virtual NodeType  getNodeType (  ) = 0;
  virtual Node_ptr  getParentNode (  ) = 0;
  virtual NodeIterator_ptr  getChildNodes (  ) = 0;
  virtual CORBA::Boolean  hasChildNodes (  ) = 0;
  virtual Node_ptr  getFirstChild (  ) = 0;
  virtual Node_ptr  getPreviousSibling (  ) = 0;
  virtual Node_ptr  getNextSibling (  ) = 0;
  virtual Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ) = 0;
  virtual Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ) = 0;
  virtual Node_ptr  removeChild ( Node_ptr  oldChild ) = 0;
  virtual void setParentNode__ ( Node_ptr  newParent ) = 0;
  virtual void setPreviousSibling__ ( Node_ptr  newPrevSib ) = 0;
  virtual void setNextSibling__ ( Node_ptr  newNextSib ) = 0;
  static Node_ptr _duplicate(Node_ptr);
  static Node_ptr _narrow(CORBA::Object_ptr);
  static Node_ptr _nil();

  static inline size_t NP_alignedSize(Node_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Node_IntfRepoID,13,initialoffset);
  }

  static inline void marshalObjRef(Node_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Node_IntfRepoID,13,s);
  }

  static inline Node_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Node_IntfRepoID,s);
    Node_ptr _result = Node::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Node_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Node_IntfRepoID,13,s);
  }

  static inline Node_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Node_IntfRepoID,s);
    Node_ptr _result = Node::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Node() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Node_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Node() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Node(const Node&);
  Node &operator=(const Node&);
  friend class _wrap_home_Node;
};

class _sk_Node :  public virtual Node {
public:

  _sk_Node() {}
  _sk_Node(const omniORB::objectKey& k);
  virtual ~_sk_Node() {}
  Node_ptr _this() { return Node::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual NodeType  getNodeType (  ) = 0;
  virtual Node_ptr  getParentNode (  ) = 0;
  virtual NodeIterator_ptr  getChildNodes (  ) = 0;
  virtual CORBA::Boolean  hasChildNodes (  ) = 0;
  virtual Node_ptr  getFirstChild (  ) = 0;
  virtual Node_ptr  getPreviousSibling (  ) = 0;
  virtual Node_ptr  getNextSibling (  ) = 0;
  virtual Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ) = 0;
  virtual Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ) = 0;
  virtual Node_ptr  removeChild ( Node_ptr  oldChild ) = 0;
  virtual void setParentNode__ ( Node_ptr  newParent ) = 0;
  virtual void setPreviousSibling__ ( Node_ptr  newPrevSib ) = 0;
  virtual void setNextSibling__ ( Node_ptr  newNextSib ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Node::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Node (const _sk_Node&);
  _sk_Node &operator=(const _sk_Node&);
};

class _proxy_Node :  public virtual Node {
public:

  _proxy_Node (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Node_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Node() {}
  virtual NodeType  getNodeType (  );
  virtual Node_ptr  getParentNode (  );
  virtual NodeIterator_ptr  getChildNodes (  );
  virtual CORBA::Boolean  hasChildNodes (  );
  virtual Node_ptr  getFirstChild (  );
  virtual Node_ptr  getPreviousSibling (  );
  virtual Node_ptr  getNextSibling (  );
  virtual Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild );
  virtual Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild );
  virtual Node_ptr  removeChild ( Node_ptr  oldChild );
  virtual void setParentNode__ ( Node_ptr  newParent );
  virtual void setPreviousSibling__ ( Node_ptr  newPrevSib );
  virtual void setNextSibling__ ( Node_ptr  newNextSib );

protected:

  _proxy_Node () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Node::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Node (const _proxy_Node&);
  _proxy_Node &operator=(const _proxy_Node&);
};

class _nil_Node : public virtual Node {
public:
  _nil_Node() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Node() {}
  NodeType  getNodeType (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeType _0RL_result = DOCUMENT;
    return _0RL_result;
#endif
  }

  Node_ptr  getParentNode (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  NodeIterator_ptr  getChildNodes (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeIterator_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  hasChildNodes (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  getFirstChild (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  getPreviousSibling (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  getNextSibling (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  removeChild ( Node_ptr  oldChild ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void setParentNode__ ( Node_ptr  newParent ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void setPreviousSibling__ ( Node_ptr  newPrevSib ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void setNextSibling__ ( Node_ptr  newNextSib ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Node::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Node :  public virtual omniLC::_lc_sk, public virtual Node {
public:

  _lc_sk_Node() {}
  _lc_sk_Node(const omniORB::objectKey& k);
  virtual ~_lc_sk_Node() {}
  Node_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual NodeType  getNodeType (  ) = 0;
  virtual Node_ptr  getParentNode (  ) = 0;
  virtual NodeIterator_ptr  getChildNodes (  ) = 0;
  virtual CORBA::Boolean  hasChildNodes (  ) = 0;
  virtual Node_ptr  getFirstChild (  ) = 0;
  virtual Node_ptr  getPreviousSibling (  ) = 0;
  virtual Node_ptr  getNextSibling (  ) = 0;
  virtual Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ) = 0;
  virtual Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ) = 0;
  virtual Node_ptr  removeChild ( Node_ptr  oldChild ) = 0;
  virtual void setParentNode__ ( Node_ptr  newParent ) = 0;
  virtual void setPreviousSibling__ ( Node_ptr  newPrevSib ) = 0;
  virtual void setNextSibling__ ( Node_ptr  newNextSib ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Node::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Node (const _lc_sk_Node&);
  _lc_sk_Node &operator=(const _lc_sk_Node&);
  Node_var _home_Node;
};

class _dead_Node : public virtual Node {
public:
  _dead_Node() { }
  virtual ~_dead_Node() {}
  Node::NodeType  getNodeType (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeType _0RL_result = DOCUMENT;
    return _0RL_result;
#endif
  }

  Node_ptr  getParentNode (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  NodeIterator_ptr  getChildNodes (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeIterator_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Boolean  hasChildNodes (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Boolean _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  getFirstChild (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  getPreviousSibling (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  getNextSibling (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Node_ptr  removeChild ( Node_ptr  oldChild ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void setParentNode__ ( Node_ptr  newParent ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void setPreviousSibling__ ( Node_ptr  newPrevSib ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void setNextSibling__ ( Node_ptr  newNextSib ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
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
    return Node::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Node :  public virtual omniLC::_wrap_home, public virtual Node {
private:
  Node_ptr _actual_Node;

protected:
  void _set_actual(Node_ptr p);
  void _release_actual();
  _wrap_home_Node() { }
public:
  _wrap_home_Node(_lc_sk_Node *sk);
  ~_wrap_home_Node();

  void _move(CORBA::Object_ptr to);
  void _remove();

  Node::NodeType  getNodeType (  ){
    return _actual_Node->getNodeType (  );
  }

  Node_ptr  getParentNode (  ){
    return _actual_Node->getParentNode (  );
  }

  NodeIterator_ptr  getChildNodes (  ){
    return _actual_Node->getChildNodes (  );
  }

  CORBA::Boolean  hasChildNodes (  ){
    return _actual_Node->hasChildNodes (  );
  }

  Node_ptr  getFirstChild (  ){
    return _actual_Node->getFirstChild (  );
  }

  Node_ptr  getPreviousSibling (  ){
    return _actual_Node->getPreviousSibling (  );
  }

  Node_ptr  getNextSibling (  ){
    return _actual_Node->getNextSibling (  );
  }

  Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ){
    return _actual_Node->insertBefore ( newChild, refChild );
  }

  Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ){
    return _actual_Node->replaceChild ( newChild, oldChild );
  }

  Node_ptr  removeChild ( Node_ptr  oldChild ){
    return _actual_Node->removeChild ( oldChild );
  }

  void setParentNode__ ( Node_ptr  newParent ){
    _actual_Node->setParentNode__ ( newParent );
  }

  void setPreviousSibling__ ( Node_ptr  newPrevSib ){
    _actual_Node->setPreviousSibling__ ( newPrevSib );
  }

  void setNextSibling__ ( Node_ptr  newNextSib ){
    _actual_Node->setNextSibling__ ( newNextSib );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Node;
class _lc_proxy_Node :  public virtual Node {
public:

  _lc_proxy_Node (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Node_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Node() {}
  void _set_wrap_Node(_wrap_proxy_Node *wrap) {
    _wrap_Node = wrap;
  }

  virtual NodeType  getNodeType (  );
  virtual Node_ptr  getParentNode (  );
  virtual NodeIterator_ptr  getChildNodes (  );
  virtual CORBA::Boolean  hasChildNodes (  );
  virtual Node_ptr  getFirstChild (  );
  virtual Node_ptr  getPreviousSibling (  );
  virtual Node_ptr  getNextSibling (  );
  virtual Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild );
  virtual Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild );
  virtual Node_ptr  removeChild ( Node_ptr  oldChild );
  virtual void setParentNode__ ( Node_ptr  newParent );
  virtual void setPreviousSibling__ ( Node_ptr  newPrevSib );
  virtual void setNextSibling__ ( Node_ptr  newNextSib );

protected:

  _lc_proxy_Node () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Node::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Node *_get_wrap_Node() {
    return _wrap_Node;
  }
  _wrap_proxy_Node *_wrap_Node;
  _lc_proxy_Node (const _lc_proxy_Node&);
  _lc_proxy_Node &operator=(const _lc_proxy_Node&);
  friend class _wrap_proxy_Node;
};

class _wrap_proxy_Node :  public virtual omniLC::_wrap_proxy, public virtual Node {
private:
  _lc_proxy_Node *_orig_Node;
  Node_var _actual_Node;

public:

  _wrap_proxy_Node (_lc_proxy_Node *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Node();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  Node::NodeType  getNodeType (  ){
    return _actual_Node->getNodeType (  );
  }

  Node_ptr  getParentNode (  ){
    return _actual_Node->getParentNode (  );
  }

  NodeIterator_ptr  getChildNodes (  ){
    return _actual_Node->getChildNodes (  );
  }

  CORBA::Boolean  hasChildNodes (  ){
    return _actual_Node->hasChildNodes (  );
  }

  Node_ptr  getFirstChild (  ){
    return _actual_Node->getFirstChild (  );
  }

  Node_ptr  getPreviousSibling (  ){
    return _actual_Node->getPreviousSibling (  );
  }

  Node_ptr  getNextSibling (  ){
    return _actual_Node->getNextSibling (  );
  }

  Node_ptr  insertBefore ( Node_ptr  newChild, Node_ptr  refChild ){
    return _actual_Node->insertBefore ( newChild, refChild );
  }

  Node_ptr  replaceChild ( Node_ptr  newChild, Node_ptr  oldChild ){
    return _actual_Node->replaceChild ( newChild, oldChild );
  }

  Node_ptr  removeChild ( Node_ptr  oldChild ){
    return _actual_Node->removeChild ( oldChild );
  }

  void setParentNode__ ( Node_ptr  newParent ){
    _actual_Node->setParentNode__ ( newParent );
  }

  void setPreviousSibling__ ( Node_ptr  newPrevSib ){
    _actual_Node->setPreviousSibling__ ( newPrevSib );
  }

  void setNextSibling__ ( Node_ptr  newNextSib ){
    _actual_Node->setNextSibling__ ( newNextSib );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Node *proxy);
  _wrap_proxy_Node() {}
};

// *** End of LifeCycle stuff
class Node_proxyObjectFactory : public proxyObjectFactory {
public:
  Node_proxyObjectFactory () {}
  virtual ~Node_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Node_ptr _nil() {
    if (!__nil_Node) {
      __nil_Node = new _nil_Node;
    }
    return __nil_Node;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Node_ptr __nil_Node;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Node;

inline void operator>>= (Node::NodeType _e,NetBufferedStream &s) {
  ::operator>>=((CORBA::ULong)_e,s);
}

inline void operator<<= (Node::NodeType &_e,NetBufferedStream &s) {
  CORBA::ULong __e;
  ::operator<<=(__e,s);
  switch (__e) {
    case Node::DOCUMENT:
    case Node::ELEMENT:
    case Node::ATTRIBUTE:
    case Node::PI:
    case Node::COMMENT:
    case Node::TEXT:
      _e = (Node::NodeType) __e;
      break;
    default:
      _CORBA_marshal_error();
  }
}

inline void operator>>= (Node::NodeType _e,MemBufferedStream &s) {
  ::operator>>=((CORBA::ULong)_e,s);
}

inline void operator<<= (Node::NodeType &_e,MemBufferedStream &s) {
  CORBA::ULong __e;
  ::operator<<=(__e,s);
  switch (__e) {
    case Node::DOCUMENT:
    case Node::ELEMENT:
    case Node::ATTRIBUTE:
    case Node::PI:
    case Node::COMMENT:
    case Node::TEXT:
      _e = (Node::NodeType) __e;
      break;
    default:
      _CORBA_marshal_error();
  }
}

void operator<<=(CORBA::Any& _a, Node::NodeType _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Node::NodeType& _s);


void operator<<=(CORBA::Any& _a, Node_ptr _s);
void operator<<=(CORBA::Any& _a, Node_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Node_ptr& _s);


#undef _LC_attr

#endif // __Node_hh__
