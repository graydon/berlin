#ifndef __Element_hh__
#define __Element_hh__

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
#ifndef __Attribute_EXTERNAL_GUARD__
#define __Attribute_EXTERNAL_GUARD__
#include <Attribute.hh>
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

#ifndef __Element__
#define __Element__
class   Element;
typedef Element* Element_ptr;
typedef Element_ptr ElementRef;

class _wrap_home_Element;

class Element_Helper {
  public:
  static Element_ptr _nil();
  static CORBA::Boolean is_nil(Element_ptr p);
  static void release(Element_ptr p);
  static void duplicate(Element_ptr p);
  static size_t NP_alignedSize(Element_ptr obj,size_t initialoffset);
  static void marshalObjRef(Element_ptr obj,NetBufferedStream &s);
  static Element_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Element_ptr obj,MemBufferedStream &s);
  static Element_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Element,Element_Helper> Element_var;

#endif
#define Element_IntfRepoID "IDL:Element:1.0"

class Element :  public virtual Node {
public:

  virtual wstring * getTagName (  ) = 0;
  virtual NodeIterator_ptr  getAttributes (  ) = 0;
  virtual wstring * getAttribute ( const wstring & name ) = 0;
  virtual void setAttribute ( const wstring & name, const wstring & value ) = 0;
  virtual void removeAttribute ( const wstring & name ) = 0;
  virtual Attrib_ptr  getAttributeNode ( const wstring & name ) = 0;
  virtual void setAttributeNode ( Attrib_ptr  newAttr ) = 0;
  virtual void removeAttributeNode ( Attrib_ptr  oldAttr ) = 0;
  virtual void getElementsByTagName ( const wstring & tagname ) = 0;
  virtual void normalize (  ) = 0;
  static Element_ptr _duplicate(Element_ptr);
  static Element_ptr _narrow(CORBA::Object_ptr);
  static Element_ptr _nil();

  static inline size_t NP_alignedSize(Element_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Element_IntfRepoID,16,initialoffset);
  }

  static inline void marshalObjRef(Element_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Element_IntfRepoID,16,s);
  }

  static inline Element_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Element_IntfRepoID,s);
    Element_ptr _result = Element::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Element_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Element_IntfRepoID,16,s);
  }

  static inline Element_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Element_IntfRepoID,s);
    Element_ptr _result = Element::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Element() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Element_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Element() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Element(const Element&);
  Element &operator=(const Element&);
  friend class _wrap_home_Element;
};

class _sk_Element :  public virtual _sk_Node, public virtual Element {
public:

  _sk_Element() {}
  _sk_Element(const omniORB::objectKey& k);
  virtual ~_sk_Element() {}
  Element_ptr _this() { return Element::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual wstring * getTagName (  ) = 0;
  virtual NodeIterator_ptr  getAttributes (  ) = 0;
  virtual wstring * getAttribute ( const wstring & name ) = 0;
  virtual void setAttribute ( const wstring & name, const wstring & value ) = 0;
  virtual void removeAttribute ( const wstring & name ) = 0;
  virtual Attrib_ptr  getAttributeNode ( const wstring & name ) = 0;
  virtual void setAttributeNode ( Attrib_ptr  newAttr ) = 0;
  virtual void removeAttributeNode ( Attrib_ptr  oldAttr ) = 0;
  virtual void getElementsByTagName ( const wstring & tagname ) = 0;
  virtual void normalize (  ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Element::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Element (const _sk_Element&);
  _sk_Element &operator=(const _sk_Element&);
};

class _proxy_Element :  public virtual _proxy_Node, public virtual Element {
public:

  _proxy_Element (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Element_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Element() {}
  virtual wstring * getTagName (  );
  virtual NodeIterator_ptr  getAttributes (  );
  virtual wstring * getAttribute ( const wstring & name );
  virtual void setAttribute ( const wstring & name, const wstring & value );
  virtual void removeAttribute ( const wstring & name );
  virtual Attrib_ptr  getAttributeNode ( const wstring & name );
  virtual void setAttributeNode ( Attrib_ptr  newAttr );
  virtual void removeAttributeNode ( Attrib_ptr  oldAttr );
  virtual void getElementsByTagName ( const wstring & tagname );
  virtual void normalize (  );

protected:

  _proxy_Element () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Element::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Element (const _proxy_Element&);
  _proxy_Element &operator=(const _proxy_Element&);
};

class _nil_Element :  public virtual _nil_Node, public virtual Element {
public:
  _nil_Element() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Element() {}
  wstring * getTagName (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  NodeIterator_ptr  getAttributes (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeIterator_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  wstring * getAttribute ( const wstring & name ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void setAttribute ( const wstring & name, const wstring & value ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void removeAttribute ( const wstring & name ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  Attrib_ptr  getAttributeNode ( const wstring & name ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void setAttributeNode ( Attrib_ptr  newAttr ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void removeAttributeNode ( Attrib_ptr  oldAttr ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void getElementsByTagName ( const wstring & tagname ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void normalize (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Element::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Element :  public virtual _lc_sk_Node, public virtual Element {
public:

  _lc_sk_Element() {}
  _lc_sk_Element(const omniORB::objectKey& k);
  virtual ~_lc_sk_Element() {}
  Element_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual wstring * getTagName (  ) = 0;
  virtual NodeIterator_ptr  getAttributes (  ) = 0;
  virtual wstring * getAttribute ( const wstring & name ) = 0;
  virtual void setAttribute ( const wstring & name, const wstring & value ) = 0;
  virtual void removeAttribute ( const wstring & name ) = 0;
  virtual Attrib_ptr  getAttributeNode ( const wstring & name ) = 0;
  virtual void setAttributeNode ( Attrib_ptr  newAttr ) = 0;
  virtual void removeAttributeNode ( Attrib_ptr  oldAttr ) = 0;
  virtual void getElementsByTagName ( const wstring & tagname ) = 0;
  virtual void normalize (  ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Element::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Element (const _lc_sk_Element&);
  _lc_sk_Element &operator=(const _lc_sk_Element&);
  Element_var _home_Element;
};

class _dead_Element :  public virtual _dead_Node, public virtual Element {
public:
  _dead_Element() { }
  virtual ~_dead_Element() {}
  wstring * getTagName (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  NodeIterator_ptr  getAttributes (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeIterator_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  wstring * getAttribute ( const wstring & name ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    wstring * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void setAttribute ( const wstring & name, const wstring & value ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void removeAttribute ( const wstring & name ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  Attrib_ptr  getAttributeNode ( const wstring & name ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void setAttributeNode ( Attrib_ptr  newAttr ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void removeAttributeNode ( Attrib_ptr  oldAttr ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void getElementsByTagName ( const wstring & tagname ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    return;
#endif
  }

  void normalize (  ){
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
    return Element::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Element :  public virtual _wrap_home_Node, public virtual Element {
private:
  Element_ptr _actual_Element;

protected:
  void _set_actual(Element_ptr p);
  void _release_actual();
  _wrap_home_Element() { }
public:
  _wrap_home_Element(_lc_sk_Element *sk);
  ~_wrap_home_Element();

  void _move(CORBA::Object_ptr to);
  void _remove();

  wstring * getTagName (  ){
    return _actual_Element->getTagName (  );
  }

  NodeIterator_ptr  getAttributes (  ){
    return _actual_Element->getAttributes (  );
  }

  wstring * getAttribute ( const wstring & name ){
    return _actual_Element->getAttribute ( name );
  }

  void setAttribute ( const wstring & name, const wstring & value ){
    _actual_Element->setAttribute ( name, value );
  }

  void removeAttribute ( const wstring & name ){
    _actual_Element->removeAttribute ( name );
  }

  Attrib_ptr  getAttributeNode ( const wstring & name ){
    return _actual_Element->getAttributeNode ( name );
  }

  void setAttributeNode ( Attrib_ptr  newAttr ){
    _actual_Element->setAttributeNode ( newAttr );
  }

  void removeAttributeNode ( Attrib_ptr  oldAttr ){
    _actual_Element->removeAttributeNode ( oldAttr );
  }

  void getElementsByTagName ( const wstring & tagname ){
    _actual_Element->getElementsByTagName ( tagname );
  }

  void normalize (  ){
    _actual_Element->normalize (  );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Element;
class _lc_proxy_Element :  public virtual _lc_proxy_Node, public virtual Element {
public:

  _lc_proxy_Element (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Element_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Element() {}
  void _set_wrap_Element(_wrap_proxy_Element *wrap) {
    _wrap_Element = wrap;
    _lc_proxy_Node::_set_wrap_Node((_wrap_proxy_Node *)wrap);
  }

  virtual wstring * getTagName (  );
  virtual NodeIterator_ptr  getAttributes (  );
  virtual wstring * getAttribute ( const wstring & name );
  virtual void setAttribute ( const wstring & name, const wstring & value );
  virtual void removeAttribute ( const wstring & name );
  virtual Attrib_ptr  getAttributeNode ( const wstring & name );
  virtual void setAttributeNode ( Attrib_ptr  newAttr );
  virtual void removeAttributeNode ( Attrib_ptr  oldAttr );
  virtual void getElementsByTagName ( const wstring & tagname );
  virtual void normalize (  );

protected:

  _lc_proxy_Element () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Element::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Element *_get_wrap_Element() {
    return _wrap_Element;
  }
  _wrap_proxy_Element *_wrap_Element;
  _lc_proxy_Element (const _lc_proxy_Element&);
  _lc_proxy_Element &operator=(const _lc_proxy_Element&);
  friend class _wrap_proxy_Element;
};

class _wrap_proxy_Element :  public virtual _wrap_proxy_Node, public virtual Element {
private:
  _lc_proxy_Element *_orig_Element;
  Element_var _actual_Element;

public:

  _wrap_proxy_Element (_lc_proxy_Element *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Element();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  wstring * getTagName (  ){
    return _actual_Element->getTagName (  );
  }

  NodeIterator_ptr  getAttributes (  ){
    return _actual_Element->getAttributes (  );
  }

  wstring * getAttribute ( const wstring & name ){
    return _actual_Element->getAttribute ( name );
  }

  void setAttribute ( const wstring & name, const wstring & value ){
    _actual_Element->setAttribute ( name, value );
  }

  void removeAttribute ( const wstring & name ){
    _actual_Element->removeAttribute ( name );
  }

  Attrib_ptr  getAttributeNode ( const wstring & name ){
    return _actual_Element->getAttributeNode ( name );
  }

  void setAttributeNode ( Attrib_ptr  newAttr ){
    _actual_Element->setAttributeNode ( newAttr );
  }

  void removeAttributeNode ( Attrib_ptr  oldAttr ){
    _actual_Element->removeAttributeNode ( oldAttr );
  }

  void getElementsByTagName ( const wstring & tagname ){
    _actual_Element->getElementsByTagName ( tagname );
  }

  void normalize (  ){
    _actual_Element->normalize (  );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Element *proxy);
  _wrap_proxy_Element() {}
};

// *** End of LifeCycle stuff
class Element_proxyObjectFactory : public proxyObjectFactory {
public:
  Element_proxyObjectFactory () {}
  virtual ~Element_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Element_ptr _nil() {
    if (!__nil_Element) {
      __nil_Element = new _nil_Element;
    }
    return __nil_Element;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Element_ptr __nil_Element;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Element;


void operator<<=(CORBA::Any& _a, Element_ptr _s);
void operator<<=(CORBA::Any& _a, Element_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Element_ptr& _s);


#undef _LC_attr

#endif // __Element_hh__
