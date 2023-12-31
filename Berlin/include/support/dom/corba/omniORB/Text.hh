#ifndef __Text_hh__
#define __Text_hh__

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

#ifndef __Text__
#define __Text__
class   Text;
typedef Text* Text_ptr;
typedef Text_ptr TextRef;

class _wrap_home_Text;

class Text_Helper {
  public:
  static Text_ptr _nil();
  static CORBA::Boolean is_nil(Text_ptr p);
  static void release(Text_ptr p);
  static void duplicate(Text_ptr p);
  static size_t NP_alignedSize(Text_ptr obj,size_t initialoffset);
  static void marshalObjRef(Text_ptr obj,NetBufferedStream &s);
  static Text_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Text_ptr obj,MemBufferedStream &s);
  static Text_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Text,Text_Helper> Text_var;

#endif
#define Text_IntfRepoID "IDL:Text:1.0"

class Text :  public virtual Node {
public:

  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual void append ( const wstring & newData ) = 0;
  virtual void insert ( CORBA::Long  offset, const wstring & newData ) = 0;
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ) = 0;
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  static Text_ptr _duplicate(Text_ptr);
  static Text_ptr _narrow(CORBA::Object_ptr);
  static Text_ptr _nil();

  static inline size_t NP_alignedSize(Text_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Text_IntfRepoID,13,initialoffset);
  }

  static inline void marshalObjRef(Text_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Text_IntfRepoID,13,s);
  }

  static inline Text_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Text_IntfRepoID,s);
    Text_ptr _result = Text::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Text_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Text_IntfRepoID,13,s);
  }

  static inline Text_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Text_IntfRepoID,s);
    Text_ptr _result = Text::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Text() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Text_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Text() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Text(const Text&);
  Text &operator=(const Text&);
  friend class _wrap_home_Text;
};

class _sk_Text :  public virtual _sk_Node, public virtual Text {
public:

  _sk_Text() {}
  _sk_Text(const omniORB::objectKey& k);
  virtual ~_sk_Text() {}
  Text_ptr _this() { return Text::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual wstring * data ()  = 0;
  virtual void data (const wstring & _value) = 0;
  virtual void append ( const wstring & newData ) = 0;
  virtual void insert ( CORBA::Long  offset, const wstring & newData ) = 0;
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ) = 0;
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Text::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Text (const _sk_Text&);
  _sk_Text &operator=(const _sk_Text&);
};

class _proxy_Text :  public virtual _proxy_Node, public virtual Text {
public:

  _proxy_Text (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Text_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Text() {}
  virtual wstring * data () ;
  virtual void data (const wstring & _value);
  virtual void append ( const wstring & newData );
  virtual void insert ( CORBA::Long  offset, const wstring & newData );
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count );
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData );
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count );
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count );

protected:

  _proxy_Text () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Text::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Text (const _proxy_Text&);
  _proxy_Text &operator=(const _proxy_Text&);
};

class _nil_Text :  public virtual _nil_Node, public virtual Text {
public:
  _nil_Text() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Text() {}
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

  void append ( const wstring & newData ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void insert ( CORBA::Long  offset, const wstring & newData ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void _delete ( CORBA::Long  offset, CORBA::Long  count ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Text_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  wstring * range ( CORBA::Long  offset, CORBA::Long  count ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Text::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Text :  public virtual _lc_sk_Node, public virtual Text {
public:

  _lc_sk_Text() {}
  _lc_sk_Text(const omniORB::objectKey& k);
  virtual ~_lc_sk_Text() {}
  Text_ptr _this();
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
  virtual void append ( const wstring & newData ) = 0;
  virtual void insert ( CORBA::Long  offset, const wstring & newData ) = 0;
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ) = 0;
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Text::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Text (const _lc_sk_Text&);
  _lc_sk_Text &operator=(const _lc_sk_Text&);
  Text_var _home_Text;
};

class _dead_Text :  public virtual _dead_Node, public virtual Text {
public:
  _dead_Text() { }
  virtual ~_dead_Text() {}
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

  void append ( const wstring & newData ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void insert ( CORBA::Long  offset, const wstring & newData ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void _delete ( CORBA::Long  offset, CORBA::Long  count ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Text_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  wstring * range ( CORBA::Long  offset, CORBA::Long  count ){
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
    return Text::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Text :  public virtual _wrap_home_Node, public virtual Text {
private:
  Text_ptr _actual_Text;

protected:
  void _set_actual(Text_ptr p);
  void _release_actual();
  _wrap_home_Text() { }
public:
  _wrap_home_Text(_lc_sk_Text *sk);
  ~_wrap_home_Text();

  void _move(CORBA::Object_ptr to);
  void _remove();

  wstring * data () {
    return _actual_Text->data();
  }

  void data (const wstring & _value){
    _actual_Text->data(_value);
  }

  void append ( const wstring & newData ){
    _actual_Text->append ( newData );
  }

  void insert ( CORBA::Long  offset, const wstring & newData ){
    _actual_Text->insert ( offset, newData );
  }

  void _delete ( CORBA::Long  offset, CORBA::Long  count ){
    _actual_Text->_delete ( offset, count );
  }

  void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ){
    _actual_Text->replace ( offset, count, newData );
  }

  Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ){
    return _actual_Text->splice ( offset, count );
  }

  wstring * range ( CORBA::Long  offset, CORBA::Long  count ){
    return _actual_Text->range ( offset, count );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Text;
class _lc_proxy_Text :  public virtual _lc_proxy_Node, public virtual Text {
public:

  _lc_proxy_Text (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Text_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Text() {}
  void _set_wrap_Text(_wrap_proxy_Text *wrap) {
    _wrap_Text = wrap;
    _lc_proxy_Node::_set_wrap_Node((_wrap_proxy_Node *)wrap);
  }

  virtual wstring * data () ;
  virtual void data (const wstring & _value);
  virtual void append ( const wstring & newData );
  virtual void insert ( CORBA::Long  offset, const wstring & newData );
  virtual void _delete ( CORBA::Long  offset, CORBA::Long  count );
  virtual void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData );
  virtual Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count );
  virtual wstring * range ( CORBA::Long  offset, CORBA::Long  count );

protected:

  _lc_proxy_Text () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Text::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Text *_get_wrap_Text() {
    return _wrap_Text;
  }
  _wrap_proxy_Text *_wrap_Text;
  _lc_proxy_Text (const _lc_proxy_Text&);
  _lc_proxy_Text &operator=(const _lc_proxy_Text&);
  friend class _wrap_proxy_Text;
};

class _wrap_proxy_Text :  public virtual _wrap_proxy_Node, public virtual Text {
private:
  _lc_proxy_Text *_orig_Text;
  Text_var _actual_Text;

public:

  _wrap_proxy_Text (_lc_proxy_Text *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Text();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  wstring * data () {
    return _actual_Text->data();
  }

  void data (const wstring & _value){
    _actual_Text->data(_value);
  }

  void append ( const wstring & newData ){
    _actual_Text->append ( newData );
  }

  void insert ( CORBA::Long  offset, const wstring & newData ){
    _actual_Text->insert ( offset, newData );
  }

  void _delete ( CORBA::Long  offset, CORBA::Long  count ){
    _actual_Text->_delete ( offset, count );
  }

  void replace ( CORBA::Long  offset, CORBA::Long  count, const wstring & newData ){
    _actual_Text->replace ( offset, count, newData );
  }

  Text_ptr  splice ( CORBA::Long  offset, CORBA::Long  count ){
    return _actual_Text->splice ( offset, count );
  }

  wstring * range ( CORBA::Long  offset, CORBA::Long  count ){
    return _actual_Text->range ( offset, count );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Text *proxy);
  _wrap_proxy_Text() {}
};

// *** End of LifeCycle stuff
class Text_proxyObjectFactory : public proxyObjectFactory {
public:
  Text_proxyObjectFactory () {}
  virtual ~Text_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Text_ptr _nil() {
    if (!__nil_Text) {
      __nil_Text = new _nil_Text;
    }
    return __nil_Text;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Text_ptr __nil_Text;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Text;


void operator<<=(CORBA::Any& _a, Text_ptr _s);
void operator<<=(CORBA::Any& _a, Text_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Text_ptr& _s);


#undef _LC_attr

#endif // __Text_hh__
