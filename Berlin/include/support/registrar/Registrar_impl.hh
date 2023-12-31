#ifndef __REGISTRAR_IMPL_HH__
#define __REGISTRAR_IMPL_HH__ 1

#include "Registrar.hh"
#include "Dcache.hh"
#include "Db.hh"

class Registrar_impl : public virtual _sk_Registrar {
private:
    Db *regdb;
    Dcache *regcache;
public:
    Registrar_impl();
    virtual ~Registrar_impl() {}

    virtual void Start(int argc, char *argv[]);

    virtual void Shutdown(int errcode);

    virtual long int createKey(const char *key, const keyaccess &perms);

    virtual long int deleteKey(const char *key);

    virtual long int getValueType(const char *key);

    virtual short int getBooleanValue(const char *key);

    virtual long int getIntegerValue(const char *key);

    virtual char *getStringValue(const char *key);

    virtual Registrar::binary *getBinaryValue(const char *key);

    virtual CORBA::Any *getValue(const char *key);

    virtual long int setBooleanValue(const char *key, short int value);

    virtual long int setIntegerValue(const char *key, long int value);

    virtual long int setStringValue(const char *key, const char *value);

    virtual long int setBinaryValue(const char *, 
                                    const Registrar::binary &value);

    virtual long int setValue(const char *key,
                              const CORBA::Any &value);

    virtual long int deleteValue(const char *key);

    virtual keypair *getKeyPair(const char *key);

    virtual keys *enumKeys(const char *key);

    virtual keyaccess getSecurityInfo(const char *key);

    virtual long int setSecurityInfo(const char *key, const keyaccess &perms);

    virtual long int watchKey(const char *key);

    virtual long int lockKey(const char *key);

    virtual long int unlockKey(const char *key);

    virtual long int isValidKeyName(const char *key);

    virtual char *getVersion(void);
};

#endif /* __REGISTRAR_IMPL_HH__ */
