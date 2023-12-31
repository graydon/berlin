#ifndef __Registrar_hh__
#define __Registrar_hh__

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
# error "A local CPP macro _LC_attr has already been defined."
#else
# ifdef  USE_stub_in_nt_dll
#  define _LC_attr _OMNIORB_NTDLL_IMPORT
# else
#  define _LC_attr
# endif
#endif

#ifndef __Registrar__
#define __Registrar__
class   Registrar;
typedef Registrar* Registrar_ptr;
typedef Registrar_ptr RegistrarRef;

class _wrap_home_Registrar;

class Registrar_Helper {
  public:
  static Registrar_ptr _nil();
  static CORBA::Boolean is_nil(Registrar_ptr p);
  static void release(Registrar_ptr p);
  static void duplicate(Registrar_ptr p);
  static size_t NP_alignedSize(Registrar_ptr obj,size_t initialoffset);
  static void marshalObjRef(Registrar_ptr obj,NetBufferedStream &s);
  static Registrar_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Registrar_ptr obj,MemBufferedStream &s);
  static Registrar_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Registrar,Registrar_Helper> Registrar_var;

#endif
#define Registrar_IntfRepoID "IDL:Registrar:1.0"

class Registrar : public virtual omniObject, public virtual CORBA::Object {
public:

  static _LC_attr const CORBA::TypeCode_ptr _tc_keys;
  typedef _CORBA_Unbounded_Sequence<CORBA::String_member > keys;
  typedef _CORBA_Sequence_Var<keys, CORBA::String_member > keys_var;

  static _LC_attr const CORBA::TypeCode_ptr _tc_values;
  typedef _CORBA_Unbounded_Sequence<CORBA::Any > values;
  typedef _CORBA_Sequence_Var<values, CORBA::Any > values_var;

  struct keyaccess {
    CORBA::Short mode;
    CORBA::Long uid;
    CORBA::Long gid;
    
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  typedef _CORBA_ConstrType_Fix_Var<keyaccess> keyaccess_var;

  static _LC_attr const CORBA::TypeCode_ptr _tc_keyaccess;

  struct keypair {
    CORBA::Any value;
    CORBA::Long type;
    CORBA::Long ctime;
    CORBA::Long mtime;
    keyaccess perms;
    
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  typedef _CORBA_ConstrType_Variable_Var<keypair> keypair_var;

  static _LC_attr const CORBA::TypeCode_ptr _tc_keypair;

  struct binary {
    CORBA::String_member value;
    CORBA::Long length;
    
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  typedef _CORBA_ConstrType_Variable_Var<binary> binary_var;

  static _LC_attr const CORBA::TypeCode_ptr _tc_binary;

  static _LC_attr const CORBA::TypeCode_ptr _tc_keypairs;
  typedef _CORBA_Unbounded_Sequence<keypair > keypairs;
  typedef _CORBA_Sequence_Var<keypairs, keypair > keypairs_var;

#define Registrar_NotFound_IntfRepoID "IDL:Registrar/NotFound:1.0"

  class NotFound : public CORBA::UserException {
  public:

    
    NotFound() {};
    NotFound(const NotFound &);
    NotFound & operator=(const NotFound &);
    virtual ~NotFound() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  static _LC_attr const CORBA::TypeCode_ptr _tc_NotFound;

#define Registrar_InvalidName_IntfRepoID "IDL:Registrar/InvalidName:1.0"

  class InvalidName : public CORBA::UserException {
  public:

    
    InvalidName() {};
    InvalidName(const InvalidName &);
    InvalidName & operator=(const InvalidName &);
    virtual ~InvalidName() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  static _LC_attr const CORBA::TypeCode_ptr _tc_InvalidName;

#define Registrar_InvalidType_IntfRepoID "IDL:Registrar/InvalidType:1.0"

  class InvalidType : public CORBA::UserException {
  public:

    
    InvalidType() {};
    InvalidType(const InvalidType &);
    InvalidType & operator=(const InvalidType &);
    virtual ~InvalidType() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  static _LC_attr const CORBA::TypeCode_ptr _tc_InvalidType;

#define Registrar_PermissionDenied_IntfRepoID "IDL:Registrar/PermissionDenied:1.0"

  class PermissionDenied : public CORBA::UserException {
  public:

    
    PermissionDenied() {};
    PermissionDenied(const PermissionDenied &);
    PermissionDenied & operator=(const PermissionDenied &);
    virtual ~PermissionDenied() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  static _LC_attr const CORBA::TypeCode_ptr _tc_PermissionDenied;

#define Registrar_EntryExists_IntfRepoID "IDL:Registrar/EntryExists:1.0"

  class EntryExists : public CORBA::UserException {
  public:

    
    EntryExists() {};
    EntryExists(const EntryExists &);
    EntryExists & operator=(const EntryExists &);
    virtual ~EntryExists() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  static _LC_attr const CORBA::TypeCode_ptr _tc_EntryExists;

#define Registrar_InternalError_IntfRepoID "IDL:Registrar/InternalError:1.0"

  class InternalError : public CORBA::UserException {
  public:

    CORBA::String_member Error;
    
    InternalError() {};
    InternalError(const InternalError &);
    InternalError(const char*  _Error);
    InternalError & operator=(const InternalError &);
    virtual ~InternalError() {};
    size_t NP_alignedSize(size_t initialoffset) const;
    void operator>>= (NetBufferedStream &) const;
    void operator<<= (NetBufferedStream &);
    void operator>>= (MemBufferedStream &) const;
    void operator<<= (MemBufferedStream &);
  };

  static _LC_attr const CORBA::TypeCode_ptr _tc_InternalError;

  virtual CORBA::Long  createKey(const char*  key, const keyaccess & perms) = 0;
  virtual CORBA::Long  deleteKey(const char*  key) = 0;
  virtual CORBA::Long  getValueType(const char*  key) = 0;
  virtual CORBA::Short  getBooleanValue(const char*  key) = 0;
  virtual CORBA::Long  getIntegerValue(const char*  key) = 0;
  virtual char*  getStringValue(const char*  key) = 0;
  virtual binary * getBinaryValue(const char*  key) = 0;
  virtual CORBA::Any * getValue(const char*  key) = 0;
  virtual CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value) = 0;
  virtual CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value) = 0;
  virtual CORBA::Long  setStringValue(const char*  key, const char*  value) = 0;
  virtual CORBA::Long  setBinaryValue(const char*  key, const binary & value) = 0;
  virtual CORBA::Long  setValue(const char*  key, const CORBA::Any & value) = 0;
  virtual CORBA::Long  deleteValue(const char*  key) = 0;
  virtual keypair * getKeyPair(const char*  key) = 0;
  virtual keys * enumKeys(const char*  key) = 0;
  virtual keyaccess  getSecurityInfo(const char*  key) = 0;
  virtual CORBA::Long  setSecurityInfo(const char*  key, const keyaccess & perms) = 0;
  virtual CORBA::Long  watchKey(const char*  key) = 0;
  virtual CORBA::Long  lockKey(const char*  key) = 0;
  virtual CORBA::Long  unlockKey(const char*  key) = 0;
  virtual char*  getVersion() = 0;
  static Registrar_ptr _duplicate(Registrar_ptr);
  static Registrar_ptr _narrow(CORBA::Object_ptr);
  static Registrar_ptr _nil();

  static inline size_t NP_alignedSize(Registrar_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Registrar_IntfRepoID,18,initialoffset);
  }

  static inline void marshalObjRef(Registrar_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Registrar_IntfRepoID,18,s);
  }

  static inline Registrar_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Registrar_IntfRepoID,s);
    Registrar_ptr _result = Registrar::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Registrar_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Registrar_IntfRepoID,18,s);
  }

  static inline Registrar_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Registrar_IntfRepoID,s);
    Registrar_ptr _result = Registrar::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Registrar() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Registrar_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Registrar() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Registrar(const Registrar&);
  Registrar &operator=(const Registrar&);
  friend class _wrap_home_Registrar;
};

class _sk_Registrar :  public virtual Registrar {
public:

  _sk_Registrar() {}
  _sk_Registrar(const omniORB::objectKey& k);
  virtual ~_sk_Registrar() {}
  Registrar_ptr _this() { return Registrar::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual CORBA::Long  createKey(const char*  key, const keyaccess & perms) = 0;
  virtual CORBA::Long  deleteKey(const char*  key) = 0;
  virtual CORBA::Long  getValueType(const char*  key) = 0;
  virtual CORBA::Short  getBooleanValue(const char*  key) = 0;
  virtual CORBA::Long  getIntegerValue(const char*  key) = 0;
  virtual char*  getStringValue(const char*  key) = 0;
  virtual binary * getBinaryValue(const char*  key) = 0;
  virtual CORBA::Any * getValue(const char*  key) = 0;
  virtual CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value) = 0;
  virtual CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value) = 0;
  virtual CORBA::Long  setStringValue(const char*  key, const char*  value) = 0;
  virtual CORBA::Long  setBinaryValue(const char*  key, const binary & value) = 0;
  virtual CORBA::Long  setValue(const char*  key, const CORBA::Any & value) = 0;
  virtual CORBA::Long  deleteValue(const char*  key) = 0;
  virtual keypair * getKeyPair(const char*  key) = 0;
  virtual keys * enumKeys(const char*  key) = 0;
  virtual keyaccess  getSecurityInfo(const char*  key) = 0;
  virtual CORBA::Long  setSecurityInfo(const char*  key, const keyaccess & perms) = 0;
  virtual CORBA::Long  watchKey(const char*  key) = 0;
  virtual CORBA::Long  lockKey(const char*  key) = 0;
  virtual CORBA::Long  unlockKey(const char*  key) = 0;
  virtual char*  getVersion() = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Registrar::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Registrar (const _sk_Registrar&);
  _sk_Registrar &operator=(const _sk_Registrar&);
};

class _proxy_Registrar :  public virtual Registrar {
public:

  _proxy_Registrar (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Registrar_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Registrar() {}
  virtual CORBA::Long  createKey(const char*  key, const keyaccess & perms);
  virtual CORBA::Long  deleteKey(const char*  key);
  virtual CORBA::Long  getValueType(const char*  key);
  virtual CORBA::Short  getBooleanValue(const char*  key);
  virtual CORBA::Long  getIntegerValue(const char*  key);
  virtual char*  getStringValue(const char*  key);
  virtual binary * getBinaryValue(const char*  key);
  virtual CORBA::Any * getValue(const char*  key);
  virtual CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value);
  virtual CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value);
  virtual CORBA::Long  setStringValue(const char*  key, const char*  value);
  virtual CORBA::Long  setBinaryValue(const char*  key, const binary & value);
  virtual CORBA::Long  setValue(const char*  key, const CORBA::Any & value);
  virtual CORBA::Long  deleteValue(const char*  key);
  virtual keypair * getKeyPair(const char*  key);
  virtual keys * enumKeys(const char*  key);
  virtual keyaccess  getSecurityInfo(const char*  key);
  virtual CORBA::Long  setSecurityInfo(const char*  key, const keyaccess & perms);
  virtual CORBA::Long  watchKey(const char*  key);
  virtual CORBA::Long  lockKey(const char*  key);
  virtual CORBA::Long  unlockKey(const char*  key);
  virtual char*  getVersion();

protected:

  _proxy_Registrar () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Registrar::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Registrar (const _proxy_Registrar&);
  _proxy_Registrar &operator=(const _proxy_Registrar&);
};

class _nil_Registrar : public virtual Registrar {
public:
  _nil_Registrar() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Registrar() {}
  CORBA::Long  createKey(const char*  key, const keyaccess & perms){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  deleteKey(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  getValueType(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Short  getBooleanValue(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Short _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  getIntegerValue(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  char*  getStringValue(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    char* _0RL_result = 0;
    return _0RL_result;
#endif
  }

  binary * getBinaryValue(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    binary * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Any * getValue(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Any * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setStringValue(const char*  key, const char*  value){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setBinaryValue(const char*  key, const binary & value){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setValue(const char*  key, const CORBA::Any & value){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  deleteValue(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  keypair * getKeyPair(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    keypair * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  keys * enumKeys(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    keys * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  keyaccess  getSecurityInfo(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    keyaccess _0RL_result;
memset((void *)&_0RL_result,0,sizeof(_0RL_result));
    return _0RL_result;
#endif
  }

  CORBA::Long  setSecurityInfo(const char*  key, const keyaccess & perms){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  watchKey(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  lockKey(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  unlockKey(const char*  key){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  char*  getVersion(){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    char* _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Registrar::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

// *** Start of LifeCycle stuff:
class _lc_sk_Registrar :  public virtual omniLC::_lc_sk, public virtual Registrar {
public:

  _lc_sk_Registrar() {}
  _lc_sk_Registrar(const omniORB::objectKey& k);
  virtual ~_lc_sk_Registrar() {}
  Registrar_ptr _this();
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual void _init_lifecycle();
  void _set_lifecycle(omniLifeCycleInfo_ptr li);
  omniLifeCycleInfo_ptr _get_lifecycle();
  virtual void _move(CORBA::Object_ptr to);
  virtual void _remove();
  virtual CORBA::Long  createKey(const char*  key, const keyaccess & perms) = 0;
  virtual CORBA::Long  deleteKey(const char*  key) = 0;
  virtual CORBA::Long  getValueType(const char*  key) = 0;
  virtual CORBA::Short  getBooleanValue(const char*  key) = 0;
  virtual CORBA::Long  getIntegerValue(const char*  key) = 0;
  virtual char*  getStringValue(const char*  key) = 0;
  virtual binary * getBinaryValue(const char*  key) = 0;
  virtual CORBA::Any * getValue(const char*  key) = 0;
  virtual CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value) = 0;
  virtual CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value) = 0;
  virtual CORBA::Long  setStringValue(const char*  key, const char*  value) = 0;
  virtual CORBA::Long  setBinaryValue(const char*  key, const binary & value) = 0;
  virtual CORBA::Long  setValue(const char*  key, const CORBA::Any & value) = 0;
  virtual CORBA::Long  deleteValue(const char*  key) = 0;
  virtual keypair * getKeyPair(const char*  key) = 0;
  virtual keys * enumKeys(const char*  key) = 0;
  virtual keyaccess  getSecurityInfo(const char*  key) = 0;
  virtual CORBA::Long  setSecurityInfo(const char*  key, const keyaccess & perms) = 0;
  virtual CORBA::Long  watchKey(const char*  key) = 0;
  virtual CORBA::Long  lockKey(const char*  key) = 0;
  virtual CORBA::Long  unlockKey(const char*  key) = 0;
  virtual char*  getVersion() = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Registrar::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
  virtual void _set_home(CORBA::Object_ptr home);
private:
  _lc_sk_Registrar (const _lc_sk_Registrar&);
  _lc_sk_Registrar &operator=(const _lc_sk_Registrar&);
  Registrar_var _home_Registrar;
};

class _dead_Registrar : public virtual Registrar {
public:
  _dead_Registrar() { }
  virtual ~_dead_Registrar() {}
  CORBA::Long  createKey(const char*  key, const Registrar::keyaccess & perms){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  deleteKey(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  getValueType(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Short  getBooleanValue(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Short _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  getIntegerValue(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  char*  getStringValue(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    char* _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Registrar::binary * getBinaryValue(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    binary * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Any * getValue(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Any * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setStringValue(const char*  key, const char*  value){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setBinaryValue(const char*  key, const Registrar::binary & value){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  setValue(const char*  key, const CORBA::Any & value){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  deleteValue(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Registrar::keypair * getKeyPair(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    keypair * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Registrar::keys * enumKeys(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    keys * _0RL_result = 0;
    return _0RL_result;
#endif
  }

  Registrar::keyaccess  getSecurityInfo(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    keyaccess _0RL_result;
memset((void *)&_0RL_result,0,sizeof(_0RL_result));
    return _0RL_result;
#endif
  }

  CORBA::Long  setSecurityInfo(const char*  key, const Registrar::keyaccess & perms){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  watchKey(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  lockKey(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  CORBA::Long  unlockKey(const char*  key){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    CORBA::Long _0RL_result = 0;
    return _0RL_result;
#endif
  }

  char*  getVersion(){
    throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    char* _0RL_result = 0;
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
    return Registrar::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class _wrap_home_Registrar :  public virtual omniLC::_wrap_home, public virtual Registrar {
private:
  Registrar_ptr _actual_Registrar;

protected:
  void _set_actual(Registrar_ptr p);
  void _release_actual();
  _wrap_home_Registrar() { }
public:
  _wrap_home_Registrar(_lc_sk_Registrar *sk);
  ~_wrap_home_Registrar();

  void _move(CORBA::Object_ptr to);
  void _remove();

  CORBA::Long  createKey(const char*  key, const Registrar::keyaccess & perms){
    return _actual_Registrar->createKey(key, perms);
  }

  CORBA::Long  deleteKey(const char*  key){
    return _actual_Registrar->deleteKey(key);
  }

  CORBA::Long  getValueType(const char*  key){
    return _actual_Registrar->getValueType(key);
  }

  CORBA::Short  getBooleanValue(const char*  key){
    return _actual_Registrar->getBooleanValue(key);
  }

  CORBA::Long  getIntegerValue(const char*  key){
    return _actual_Registrar->getIntegerValue(key);
  }

  char*  getStringValue(const char*  key){
    return _actual_Registrar->getStringValue(key);
  }

  Registrar::binary * getBinaryValue(const char*  key){
    return _actual_Registrar->getBinaryValue(key);
  }

  CORBA::Any * getValue(const char*  key){
    return _actual_Registrar->getValue(key);
  }

  CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value){
    return _actual_Registrar->setBooleanValue(key, value);
  }

  CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value){
    return _actual_Registrar->setIntegerValue(key, value);
  }

  CORBA::Long  setStringValue(const char*  key, const char*  value){
    return _actual_Registrar->setStringValue(key, value);
  }

  CORBA::Long  setBinaryValue(const char*  key, const Registrar::binary & value){
    return _actual_Registrar->setBinaryValue(key, value);
  }

  CORBA::Long  setValue(const char*  key, const CORBA::Any & value){
    return _actual_Registrar->setValue(key, value);
  }

  CORBA::Long  deleteValue(const char*  key){
    return _actual_Registrar->deleteValue(key);
  }

  Registrar::keypair * getKeyPair(const char*  key){
    return _actual_Registrar->getKeyPair(key);
  }

  Registrar::keys * enumKeys(const char*  key){
    return _actual_Registrar->enumKeys(key);
  }

  Registrar::keyaccess  getSecurityInfo(const char*  key){
    return _actual_Registrar->getSecurityInfo(key);
  }

  CORBA::Long  setSecurityInfo(const char*  key, const Registrar::keyaccess & perms){
    return _actual_Registrar->setSecurityInfo(key, perms);
  }

  CORBA::Long  watchKey(const char*  key){
    return _actual_Registrar->watchKey(key);
  }

  CORBA::Long  lockKey(const char*  key){
    return _actual_Registrar->lockKey(key);
  }

  CORBA::Long  unlockKey(const char*  key){
    return _actual_Registrar->unlockKey(key);
  }

  char*  getVersion(){
    return _actual_Registrar->getVersion();
  }

  CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response) {
    return _dispatcher->dispatch(s, op, response);
  };
  void _obj_is_ready(CORBA::BOA_ptr boa) {
    boa->obj_is_ready(this);
  };
};

class _wrap_proxy_Registrar;
class _lc_proxy_Registrar :  public virtual Registrar {
public:

  _lc_proxy_Registrar (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Registrar_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_lc_proxy_Registrar() {}
  void _set_wrap_Registrar(_wrap_proxy_Registrar *wrap) {
    _wrap_Registrar = wrap;
  }

  virtual CORBA::Long  createKey(const char*  key, const keyaccess & perms);
  virtual CORBA::Long  deleteKey(const char*  key);
  virtual CORBA::Long  getValueType(const char*  key);
  virtual CORBA::Short  getBooleanValue(const char*  key);
  virtual CORBA::Long  getIntegerValue(const char*  key);
  virtual char*  getStringValue(const char*  key);
  virtual binary * getBinaryValue(const char*  key);
  virtual CORBA::Any * getValue(const char*  key);
  virtual CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value);
  virtual CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value);
  virtual CORBA::Long  setStringValue(const char*  key, const char*  value);
  virtual CORBA::Long  setBinaryValue(const char*  key, const binary & value);
  virtual CORBA::Long  setValue(const char*  key, const CORBA::Any & value);
  virtual CORBA::Long  deleteValue(const char*  key);
  virtual keypair * getKeyPair(const char*  key);
  virtual keys * enumKeys(const char*  key);
  virtual keyaccess  getSecurityInfo(const char*  key);
  virtual CORBA::Long  setSecurityInfo(const char*  key, const keyaccess & perms);
  virtual CORBA::Long  watchKey(const char*  key);
  virtual CORBA::Long  lockKey(const char*  key);
  virtual CORBA::Long  unlockKey(const char*  key);
  virtual char*  getVersion();

protected:

  _lc_proxy_Registrar () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0) {
    return Registrar::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _wrap_proxy_Registrar *_get_wrap_Registrar() {
    return _wrap_Registrar;
  }
  _wrap_proxy_Registrar *_wrap_Registrar;
  _lc_proxy_Registrar (const _lc_proxy_Registrar&);
  _lc_proxy_Registrar &operator=(const _lc_proxy_Registrar&);
  friend class _wrap_proxy_Registrar;
};

class _wrap_proxy_Registrar :  public virtual omniLC::_wrap_proxy, public virtual Registrar {
private:
  _lc_proxy_Registrar *_orig_Registrar;
  Registrar_var _actual_Registrar;

public:

  _wrap_proxy_Registrar (_lc_proxy_Registrar *proxy,Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual ~_wrap_proxy_Registrar();
  virtual void _forward_to(CORBA::Object_ptr obj);
  virtual void _reset_proxy();

  CORBA::Long  createKey(const char*  key, const Registrar::keyaccess & perms){
    return _actual_Registrar->createKey(key, perms);
  }

  CORBA::Long  deleteKey(const char*  key){
    return _actual_Registrar->deleteKey(key);
  }

  CORBA::Long  getValueType(const char*  key){
    return _actual_Registrar->getValueType(key);
  }

  CORBA::Short  getBooleanValue(const char*  key){
    return _actual_Registrar->getBooleanValue(key);
  }

  CORBA::Long  getIntegerValue(const char*  key){
    return _actual_Registrar->getIntegerValue(key);
  }

  char*  getStringValue(const char*  key){
    return _actual_Registrar->getStringValue(key);
  }

  Registrar::binary * getBinaryValue(const char*  key){
    return _actual_Registrar->getBinaryValue(key);
  }

  CORBA::Any * getValue(const char*  key){
    return _actual_Registrar->getValue(key);
  }

  CORBA::Long  setBooleanValue(const char*  key, CORBA::Short  value){
    return _actual_Registrar->setBooleanValue(key, value);
  }

  CORBA::Long  setIntegerValue(const char*  key, CORBA::Long  value){
    return _actual_Registrar->setIntegerValue(key, value);
  }

  CORBA::Long  setStringValue(const char*  key, const char*  value){
    return _actual_Registrar->setStringValue(key, value);
  }

  CORBA::Long  setBinaryValue(const char*  key, const Registrar::binary & value){
    return _actual_Registrar->setBinaryValue(key, value);
  }

  CORBA::Long  setValue(const char*  key, const CORBA::Any & value){
    return _actual_Registrar->setValue(key, value);
  }

  CORBA::Long  deleteValue(const char*  key){
    return _actual_Registrar->deleteValue(key);
  }

  Registrar::keypair * getKeyPair(const char*  key){
    return _actual_Registrar->getKeyPair(key);
  }

  Registrar::keys * enumKeys(const char*  key){
    return _actual_Registrar->enumKeys(key);
  }

  Registrar::keyaccess  getSecurityInfo(const char*  key){
    return _actual_Registrar->getSecurityInfo(key);
  }

  CORBA::Long  setSecurityInfo(const char*  key, const Registrar::keyaccess & perms){
    return _actual_Registrar->setSecurityInfo(key, perms);
  }

  CORBA::Long  watchKey(const char*  key){
    return _actual_Registrar->watchKey(key);
  }

  CORBA::Long  lockKey(const char*  key){
    return _actual_Registrar->lockKey(key);
  }

  CORBA::Long  unlockKey(const char*  key){
    return _actual_Registrar->unlockKey(key);
  }

  char*  getVersion(){
    return _actual_Registrar->getVersion();
  }


protected:

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type = 0);
  virtual void _set_actual(CORBA::Object_ptr p);
  void _set_proxy(_lc_proxy_Registrar *proxy);
  _wrap_proxy_Registrar() {}
};

// *** End of LifeCycle stuff
class Registrar_proxyObjectFactory : public proxyObjectFactory {
public:
  Registrar_proxyObjectFactory () {}
  virtual ~Registrar_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Registrar_ptr _nil() {
    if (!__nil_Registrar) {
      __nil_Registrar = new _nil_Registrar;
    }
    return __nil_Registrar;
  }
  static void mayMoveLocal(CORBA::Boolean l) { _may_move_local = l; }
private:
  static Registrar_ptr __nil_Registrar;
  static CORBA::Boolean _may_move_local;
};

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_Registrar;

#ifndef __0RL_seq_any_s0_cstring__
#define __0RL_seq_any_s0_cstring__
#define __0RL_seq_anyimpl_Registrar_mkeys__
extern void _0RL_seq_anyinsert_Registrar_mkeys(CORBA::Any&, const _CORBA_Unbounded_Sequence<CORBA::String_member >&);

inline void operator<<=(CORBA::Any& a, const _CORBA_Unbounded_Sequence<CORBA::String_member >& s)
{
  _0RL_seq_anyinsert_Registrar_mkeys(a, s);
}

inline void operator<<=(CORBA::Any& a, _CORBA_Unbounded_Sequence<CORBA::String_member >* sp)
{
  _0RL_seq_anyinsert_Registrar_mkeys(a, *sp);
  delete sp;
}

extern CORBA::Boolean _0RL_seq_anyextract_Registrar_mkeys(const CORBA::Any&, _CORBA_Unbounded_Sequence<CORBA::String_member >*&);

inline CORBA::Boolean operator>>=(const CORBA::Any& a, _CORBA_Unbounded_Sequence<CORBA::String_member >*& sp)
{
  return _0RL_seq_anyextract_Registrar_mkeys(a, sp);
}

#endif

#ifndef __0RL_seq_any_s0_cany__
#define __0RL_seq_any_s0_cany__
#define __0RL_seq_anyimpl_Registrar_mvalues__
extern void _0RL_seq_anyinsert_Registrar_mvalues(CORBA::Any&, const _CORBA_Unbounded_Sequence<CORBA::Any >&);

inline void operator<<=(CORBA::Any& a, const _CORBA_Unbounded_Sequence<CORBA::Any >& s)
{
  _0RL_seq_anyinsert_Registrar_mvalues(a, s);
}

inline void operator<<=(CORBA::Any& a, _CORBA_Unbounded_Sequence<CORBA::Any >* sp)
{
  _0RL_seq_anyinsert_Registrar_mvalues(a, *sp);
  delete sp;
}

extern CORBA::Boolean _0RL_seq_anyextract_Registrar_mvalues(const CORBA::Any&, _CORBA_Unbounded_Sequence<CORBA::Any >*&);

inline CORBA::Boolean operator>>=(const CORBA::Any& a, _CORBA_Unbounded_Sequence<CORBA::Any >*& sp)
{
  return _0RL_seq_anyextract_Registrar_mvalues(a, sp);
}

#endif

extern void operator<<=(CORBA::Any& _a, const Registrar::keyaccess& _s);
extern void operator<<=(CORBA::Any& _a, Registrar::keyaccess* _sp);
extern CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::keyaccess*& _sp);

extern void operator<<=(CORBA::Any& _a, const Registrar::keypair& _s);
extern void operator<<=(CORBA::Any& _a, Registrar::keypair* _sp);
extern CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::keypair*& _sp);

extern void operator<<=(CORBA::Any& _a, const Registrar::binary& _s);
extern void operator<<=(CORBA::Any& _a, Registrar::binary* _sp);
extern CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::binary*& _sp);

#ifndef __0RL_seq_any_s0_cRegistrar_mkeypair__
#define __0RL_seq_any_s0_cRegistrar_mkeypair__
#define __0RL_seq_anyimpl_Registrar_mkeypairs__
extern void _0RL_seq_anyinsert_Registrar_mkeypairs(CORBA::Any&, const _CORBA_Unbounded_Sequence<Registrar::keypair >&);

inline void operator<<=(CORBA::Any& a, const _CORBA_Unbounded_Sequence<Registrar::keypair >& s)
{
  _0RL_seq_anyinsert_Registrar_mkeypairs(a, s);
}

inline void operator<<=(CORBA::Any& a, _CORBA_Unbounded_Sequence<Registrar::keypair >* sp)
{
  _0RL_seq_anyinsert_Registrar_mkeypairs(a, *sp);
  delete sp;
}

extern CORBA::Boolean _0RL_seq_anyextract_Registrar_mkeypairs(const CORBA::Any&, _CORBA_Unbounded_Sequence<Registrar::keypair >*&);

inline CORBA::Boolean operator>>=(const CORBA::Any& a, _CORBA_Unbounded_Sequence<Registrar::keypair >*& sp)
{
  return _0RL_seq_anyextract_Registrar_mkeypairs(a, sp);
}

#endif

void operator<<=(CORBA::Any& _a, const Registrar::NotFound& _s);
void operator<<=(CORBA::Any& _a, Registrar::NotFound* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::NotFound*& _sp);

void operator<<=(CORBA::Any& _a, const Registrar::InvalidName& _s);
void operator<<=(CORBA::Any& _a, Registrar::InvalidName* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::InvalidName*& _sp);

void operator<<=(CORBA::Any& _a, const Registrar::InvalidType& _s);
void operator<<=(CORBA::Any& _a, Registrar::InvalidType* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::InvalidType*& _sp);

void operator<<=(CORBA::Any& _a, const Registrar::PermissionDenied& _s);
void operator<<=(CORBA::Any& _a, Registrar::PermissionDenied* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::PermissionDenied*& _sp);

void operator<<=(CORBA::Any& _a, const Registrar::EntryExists& _s);
void operator<<=(CORBA::Any& _a, Registrar::EntryExists* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::EntryExists*& _sp);

void operator<<=(CORBA::Any& _a, const Registrar::InternalError& _s);
void operator<<=(CORBA::Any& _a, Registrar::InternalError* _sp);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar::InternalError*& _sp);


void operator<<=(CORBA::Any& _a, Registrar_ptr _s);
CORBA::Boolean operator>>=(const CORBA::Any& _a, Registrar_ptr& _s);


#undef _LC_attr

#endif // __Registrar_hh__
