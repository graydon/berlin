#ifndef __Document_hh__
#define __Document_hh__

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
#ifndef __AttributeList_EXTERNAL_GUARD__
#define __AttributeList_EXTERNAL_GUARD__
#include <AttributeList.hh>
#endif
#ifndef __Element_EXTERNAL_GUARD__
#define __Element_EXTERNAL_GUARD__
#include <Element.hh>
#endif
#ifndef __Text_EXTERNAL_GUARD__
#define __Text_EXTERNAL_GUARD__
#include <Text.hh>
#endif
#ifndef __Comment_EXTERNAL_GUARD__
#define __Comment_EXTERNAL_GUARD__
#include <Comment.hh>
#endif
#ifndef __ProcessingInstruction_EXTERNAL_GUARD__
#define __ProcessingInstruction_EXTERNAL_GUARD__
#include <ProcessingInstruction.hh>
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

#ifndef __Document__
#define __Document__
class   Document;
typedef Document* Document_ptr;
typedef Document_ptr DocumentRef;

class _wrap_home_Document;

class Document_Helper {
  public:
  static Document_ptr _nil();
  static CORBA::Boolean is_nil(Document_ptr p);
  static void release(Document_ptr p);
  static void duplicate(Document_ptr p);
  static size_t NP_alignedSize(Document_ptr obj,size_t initialoffset);
  static void marshalObjRef(Document_ptr obj,NetBufferedStream &s);
  static Document_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Document_ptr obj,MemBufferedStream &s);
  static Document_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Document,Document_Helper> Document_var;

#endif
#define Document_IntfRepoID "IDL:Document:1.0"

class Document :  public virtual Node {
public:

  virtual Node_ptr  documentType ()  = 0;
  virtual void documentType (Node_ptr  _value) = 0;
  virtual Element_ptr  documentElement ()  = 0;
  virtual void documentElement (Element_ptr  _value) = 0;
  virtual Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ) = 0;
  virtual Text_ptr  createTextNode ( const wstring & data ) = 0;
  virtual Comment_ptr  createComment ( const wstring & data ) = 0;
  virtual ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ) = 0;
  virtual Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ) = 0;
  virtual AttributeList_ptr  createAttributeList (  ) = 0;
  virtual NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ) = 0;
  static Document_ptr _duplicate(Document_ptr);
  static Document_ptr _narrow(CORBA::Object_ptr);
  static Document_ptr _nil();

  static inline size_t NP_alignedSize(Document_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Document_IntfRepoID,17,initialoffset);
  }

  static inline void marshalObjRef(Document_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Document_IntfRepoID,17,s);
  }

  static inline Document_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Document_IntfRepoID,s);
    Document_ptr _result = Document::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Document_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Document_IntfRepoID,17,s);
  }

  static inline Document_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Document_IntfRepoID,s);
    Document_ptr _result = Document::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Document() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Document_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Document() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Document(const Document&);
  Document &operator=(const Document&);
  friend class _wrap_home_Document;
};

class _sk_Document :  public virtual _sk_Node, public virtual Document {
public:

  _sk_Document() {}
  _sk_Document(const omniORB::objectKey& k);
  virtual ~_sk_Document() {}
  Document_ptr _this() { return Document::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual Node_ptr  documentType ()  = 0;
  virtual void documentType (Node_ptr  _value) = 0;
  virtual Element_ptr  documentElement ()  = 0;
  virtual void documentElement (Element_ptr  _value) = 0;
  virtual Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ) = 0;
  virtual Text_ptr  createTextNode ( const wstring & data ) = 0;
  virtual Comment_ptr  createComment ( const wstring & data ) = 0;
  virtual ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ) = 0;
  virtual Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ) = 0;
  virtual AttributeList_ptr  createAttributeList (  ) = 0;
  virtual NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Document::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Document (const _sk_Document&);
  _sk_Document &operator=(const _sk_Document&);
};

class _proxy_Document :  public virtual _proxy_Node, public virtual Document {
public:

  _proxy_Document (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Document_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Document() {}
  virtual Node_ptr  documentType () ;
  virtual void documentType (Node_ptr  _value);
  virtual Element_ptr  documentElement () ;
  virtual void documentElement (Element_ptr  _value);
  virtual Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes );
  virtual Text_ptr  createTextNode ( const wstring & data );
  virtual Comment_ptr  createComment ( const wstring & data );
  virtual ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data );
  virtual Attrib_ptr  createAttribute ( const wstring & name, const wstring & value );
  virtual AttributeList_ptr  createAttributeList (  );
  virtual NodeIterator_ptr  getElementsByTagName ( const wstring & tagname );

protected:

  _proxy_Document () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Document::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Document (const _proxy_Document&);
  _proxy_Document &operator=(const _proxy_Document&);
};

class _nil_Document :  public virtual _nil_Node, public virtual Document {
public:
  _nil_Document() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Document() {}
  Node_ptr  documentType ()  {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void documentType (Node_ptr  _value) {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
  }

  Element_ptr  documentElement ()  {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Element_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void documentElement (Element_ptr  _value) {
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
  }

  Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Element_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Text_ptr  createTextNode ( const wstring & data ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Text_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Comment_ptr  createComment ( const wstring & data ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Comment_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    ProcessingInstruction_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  AttributeList_ptr  createAttributeList (  ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    AttributeList_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeIterator_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Document::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Document :  public virtual _lc_sk_Node, public virtual Document {
public:

  _lc_sk_Document() {}
  _lc_sk_Document(const omniORB::objectKey& k);
  virtual ~_lc_sk_Document() {}
  Document_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual Node_ptr  documentType ()  = 0;
  virtual void documentType (Node_ptr  _value) = 0;
  virtual Element_ptr  documentElement ()  = 0;
  virtual void documentElement (Element_ptr  _value) = 0;
  virtual Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ) = 0;
  virtual Text_ptr  createTextNode ( const wstring & data ) = 0;
  virtual Comment_ptr  createComment ( const wstring & data ) = 0;
  virtual ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ) = 0;
  virtual Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ) = 0;
  virtual AttributeList_ptr  createAttributeList (  ) = 0;
  virtual NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ) = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Document::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Document (const _lc_sk_Document&);
  _lc_sk_Document &operator=(const _lc_sk_Document&);
  Document_var _home_Document;
};

class _dead_Document :  public virtual _dead_Node, public virtual Document {
public:
  _dead_Document() { }
  virtual ~_dead_Document() {}
  Node_ptr  documentType ()  {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Node_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void documentType (Node_ptr  _value) {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
  }

  Element_ptr  documentElement ()  {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Element_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  void documentElement (Element_ptr  _value) {
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
  }

  Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Element_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Text_ptr  createTextNode ( const wstring & data ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Text_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Comment_ptr  createComment ( const wstring & data ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Comment_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    ProcessingInstruction_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Attrib_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  AttributeList_ptr  createAttributeList (  ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    AttributeList_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

  NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    NodeIterator_ptr _0RL_result = 0;
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
    return Document::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Document :  public virtual _wrap_home_Node, public virtual Document {
private:
  Document_ptr _actual_Document;

protected:
  void _set_actual(Document_ptr p);
  void _release_actual();
  _wrap_home_Document() { }
public:
  _wrap_home_Document(_lc_sk_Document *sk);
  ~_wrap_home_Document();

  void _move(CORBA::Object_ptr to);
  void _remove();

  Node_ptr  documentType () {
    return _actual_Document->documentType();
  }

  void documentType (Node_ptr  _value){
    _actual_Document->documentType(_value);
  }

  Element_ptr  documentElement () {
    return _actual_Document->documentElement();
  }

  void documentElement (Element_ptr  _value){
    _actual_Document->documentElement(_value);
  }

  Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ){
    return _actual_Document->createElement ( tagName, attributes );
  }

  Text_ptr  createTextNode ( const wstring & data ){
    return _actual_Document->createTextNode ( data );
  }

  Comment_ptr  createComment ( const wstring & data ){
    return _actual_Document->createComment ( data );
  }

  ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ){
    return _actual_Document->createProcessingInstruction ( name, data );
  }

  Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ){
    return _actual_Document->createAttribute ( name, value );
  }

  AttributeList_ptr  createAttributeList (  ){
    return _actual_Document->createAttributeList (  );
  }

  NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ){
    return _actual_Document->getElementsByTagName ( tagname );
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Document;
class _lc_proxy_Document :  public virtual _lc_proxy_Node, public virtual Document {
public:

  _lc_proxy_Document (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Document_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Document() {}
  void _set_wrap_Document(_wrap_proxy_Document *wrap) {
    _wrap_Document = wrap;
    _lc_proxy_Node::_set_wrap_Node((_wrap_proxy_Node *)wrap);
  }

  virtual Node_ptr  documentType () ;
  virtual void documentType (Node_ptr  _value);
  virtual Element_ptr  documentElement () ;
  virtual void documentElement (Element_ptr  _value);
  virtual Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes );
  virtual Text_ptr  createTextNode ( const wstring & data );
  virtual Comment_ptr  createComment ( const wstring & data );
  virtual ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data );
  virtual Attrib_ptr  createAttribute ( const wstring & name, const wstring & value );
  virtual AttributeList_ptr  createAttributeList (  );
  virtual NodeIterator_ptr  getElementsByTagName ( const wstring & tagname );

protected:

  _lc_proxy_Document () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Document::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Document *_get_wrap_Document() {
    return _wrap_Document;
  }
  _wrap_proxy_Document *_wrap_Document;
  _lc_proxy_Document (const _lc_proxy_Document&);
  _lc_proxy_Document &operator=(const _lc_proxy_Document&);
  friend class _wrap_proxy_Document;
};

class _wrap_proxy_Document :  public virtual _wrap_proxy_Node, public virtual Document {
private:
  _lc_proxy_Document *_orig_Document;
  Document_var _actual_Document;

public:

  _wrap_proxy_Document (_lc_proxy_Document *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Document();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  Node_ptr  documentType () {
    return _actual_Document->documentType();
  }

  void documentType (Node_ptr  _value){
    _actual_Document->documentType(_value);
  }

  Element_ptr  documentElement () {
    return _actual_Document->documentElement();
  }

  void documentElement (Element_ptr  _value){
    _actual_Document->documentElement(_value);
  }

  Element_ptr  createElement ( const wstring & tagName, AttributeList_ptr  attributes ){
    return _actual_Document->createElement ( tagName, attributes );
  }

  Text_ptr  createTextNode ( const wstring & data ){
    return _actual_Document->createTextNode ( data );
  }

  Comment_ptr  createComment ( const wstring & data ){
    return _actual_Document->createComment ( data );
  }

  ProcessingInstruction_ptr  createProcessingInstruction ( const wstring & name, const wstring & data ){
    return _actual_Document->createProcessingInstruction ( name, data );
  }

  Attrib_ptr  createAttribute ( const wstring & name, const wstring & value ){
    return _actual_Document->createAttribute ( name, value );
  }

  AttributeList_ptr  createAttributeList (  ){
    return _actual_Document->createAttributeList (  );
  }

  NodeIterator_ptr  getElementsByTagName ( const wstring & tagname ){
    return _actual_Document->getElementsByTagName ( tagname );
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Document *proxy);
  _wrap_proxy_Document() {}
};

// *** End of LifeCycle stuff
class Document_proxyObjectFactory : public proxyObjectFactory {
public:
  Document_proxyObjectFactory () {}
  virtual ~Document_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Document_ptr _nil() {
    if (!__nil_Document) {
      __nil_Document = new _nil_Document;
    }
    return __nil_Document;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Document_ptr __nil_Document;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Document;


void operator<<=(CORBA::Any& _a, Document_ptr _s);
void operator<<=(CORBA::Any& _a, Document_ptr* _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Document_ptr& _s);


#undef _LC_attr

#endif // __Document_hh__
