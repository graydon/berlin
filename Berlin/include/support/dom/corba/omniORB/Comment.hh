#ifndef __Comment_hh__
#define __Comment_hh__

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

#ifndef __Comment__
#define __Comment__
class   Comment;
typedef Comment* Comment_ptr;
typedef Comment_ptr CommentRef;

class _wrap_home_Comment;

class Comment_Helper {
  public:
  static Comment_ptr _nil();
  static CORBA::Boolean is_nil(Comment_ptr p);
  static void release(Comment_ptr p);
  static void duplicate(Comment_ptr p);
  static size_t NP_alignedSize(Comment_ptr obj,size_t initialoffset);
  static void marshalObjRef(Comment_ptr obj,NetBufferedStream &s);
  static Comment_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Comment_ptr obj,MemBufferedStream &s);
  static Comment_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Comment,Comment_Helper> Comment_var;

#endif
#define Comment_IntfRepoID "IDL:Comment:1.0"

class Comment :  public virtual Node {
public:

  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  static Comment_ptr _duplicate(Comment_ptr);
  static Comment_ptr _narrow(CORBA::Object_ptr);
  static Comment_ptr _nil();

  static inline size_t NP_alignedSize(Comment_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Comment_IntfRepoID,16,initialoffset);
  }

  static inline void marshalObjRef(Comment_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Comment_IntfRepoID,16,s);
  }

  static inline Comment_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Comment_IntfRepoID,s);
    Comment_ptr _result = Comment::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Comment_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Comment_IntfRepoID,16,s);
  }

  static inline Comment_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Comment_IntfRepoID,s);
    Comment_ptr _result = Comment::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Comment() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Comment_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Comment() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Comment(const Comment&);
  Comment &operator=(const Comment&);
  friend class _wrap_home_Comment;
};

class _sk_Comment :  public virtual _sk_Node, public virtual Comment {
public:

  _sk_Comment() {}
  _sk_Comment(const omniORB::objectKey& k);
  virtual ~_sk_Comment() {}
  Comment_ptr _this() { return Comment::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Comment::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Comment (const _sk_Comment&);
  _sk_Comment &operator=(const _sk_Comment&);
};

class _proxy_Comment :  public virtual _proxy_Node, public virtual Comment {
public:

  _proxy_Comment (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Comment_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Comment() {}
  virtual wstring * data () ;
  virtual void data (const wstring & _value);

protected:

  _proxy_Comment () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Comment::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Comment (const _proxy_Comment&);
  _proxy_Comment &operator=(const _proxy_Comment&);
};

class _nil_Comment :  public virtual _nil_Node, public virtual Comment {
public:
  _nil_Comment() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Comment() {}
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
    return Comment::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Comment :  public virtual _lc_sk_Node, public virtual Comment {
public:

  _lc_sk_Comment() {}
  _lc_sk_Comment(const omniORB::objectKey& k);
  virtual ~_lc_sk_Comment() {}
  Comment_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Comment::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Comment (const _lc_sk_Comment&);
  _lc_sk_Comment &operator=(const _lc_sk_Comment&);
  Comment_var _home_Comment;
};

class _dead_Comment :  public virtual _dead_Node, public virtual Comment {
public:
  _dead_Comment() { }
  virtual ~_dead_Comment() {}
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
    return Comment::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Comment :  public virtual _wrap_home_Node, public virtual Comment {
private:
  Comment_ptr _actual_Comment;

protected:
  void _set_actual(Comment_ptr p);
  void _release_actual();
  _wrap_home_Comment() { }
public:
  _wrap_home_Comment(_lc_sk_Comment *sk);
  ~_wrap_home_Comment();

  void _move(CORBA::Object_ptr to);
  void _remove();

  wstring * data () {
    return _actual_Comment->data();
  }

  void data (const wstring & _value){
    _actual_Comment->data(_value);
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Comment;
class _lc_proxy_Comment :  public virtual _lc_proxy_Node, public virtual Comment {
public:

  _lc_proxy_Comment (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Comment_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Comment() {}
  void _set_wrap_Comment(_wrap_proxy_Comment *wrap) {
    _wrap_Comment = wrap;
    _lc_proxy_Node::_set_wrap_Node((_wrap_proxy_Node *)wrap);
  }

  virtual wstring * data () ;
  virtual void data (const wstring & _value);

protected:

  _lc_proxy_Comment () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Comment::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Comment *_get_wrap_Comment() {
    return _wrap_Comment;
  }
  _wrap_proxy_Comment *_wrap_Comment;
  _lc_proxy_Comment (const _lc_proxy_Comment&);
  _lc_proxy_Comment &operator=(const _lc_proxy_Comment&);
  friend class _wrap_proxy_Comment;
};

class _wrap_proxy_Comment :  public virtual _wrap_proxy_Node, public virtual Comment {
private:
  _lc_proxy_Comment *_orig_Comment;
  Comment_var _actual_Comment;

public:

  _wrap_proxy_Comment (_lc_proxy_Comment *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Comment();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  wstring * data () {
    return _actual_Comment->data();
  }

  void data (const wstring & _value){
    _actual_Comment->data(_value);
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Comment *proxy);
  _wrap_proxy_Comment() {}
};

// *** End of LifeCycle stuff
class Comment_proxyObjectFactory : public proxyObjectFactory {
public:
  Comment_proxyObjectFactory () {}
  virtual ~Comment_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Comment_ptr _nil() {
    if (!__nil_Comment) {
      __nil_Comment = new _nil_Comment;
    }
    return __nil_Comment;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Comment_ptr __nil_Comment;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Comment;


void operator<<=(CORBA::Any& _a, Comment_ptr _s);
void operator<<=(CORBA::Any& _a, Comment_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Comment_ptr& _s);


#undef _LC_attr

#endif // __Comment_hh__
