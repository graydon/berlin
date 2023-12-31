#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <syslog.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <iostream.h>
#include <sys/types.h>

#include "support/registrar/RegistrarDefs.hh"
#include "support/registrar/Registrar_impl.hh"

Registrar_impl::Registrar_impl() {
    return;
}

void Registrar_impl::Start(int argc, char *argv[]) {
    FILE *fd;
    pid_t npid = getpid();

    openlog("registrar", LOG_PID | LOG_NDELAY, LOG_DAEMON);

    syslog(LOG_ERR, "Starting Registrar");

    unlink(REGISTRAR_PID);
    fd = fopen(REGISTRAR_PID, "w");
    if (fd != NULL) {
        fprintf(fd, "%u\n", npid);
        if (fflush(fd)) {
            fclose(fd);
            unlink(REGISTRAR_PID);
            cerr << "fflush :" << strerror(errno) << endl;
        }
        fclose(fd);
    } else
        cerr << "fopen :" << strerror(errno) << endl;

    // Try initting the database
    regdb = new Db();
    regdb->initDb();

    regcache = new Dcache();
}

void Registrar_impl::Shutdown(int errcode) {
    syslog(LOG_ERR, "Shuting down with errorcode %d", errcode);
    regdb->closeDb();
    exit(errcode);
}

long int Registrar_impl::createKey(const char *key, const keyaccess &perms) {
    registrar_key rkey;
    char *keyname, *tkey;
    int32_t offset, parent_offset;

    isValidKeyName(key);

    tkey = CORBA::string_dup(key);

    if ((offset = regdb->getKeyOffsetByName(tkey)) != 0) {
        registrar_key trkey = regdb->getKey(offset);
        throw Registrar::EntryExists();
    }

    keyname = strrchr(tkey, '/');
    *keyname = '\0';
    keyname++;

    strcpy(rkey.name, keyname);

    rkey.mode = perms.mode;
    rkey.uid = perms.uid;
    rkey.gid = perms.gid;
    rkey.ctime = rkey.mtime = time(0);
    rkey.dtime = 0;
    rkey.type = REG_TYPE_NULL;

    rkey.value_size = 0;
    rkey.value_offset = 0;    /* when there is no value, offset = 0 */
    rkey.key_offset = 0;      /* there are no keys past this */
    rkey.subkey_offset = 0;   /* ditto for subkeys */

    offset = regdb->addKey(&rkey);
    parent_offset = regdb->getKeyOffsetByName(tkey);

    if (parent_offset == 0)
        return 0;

    regcache->deleteKey(tkey);
    regdb->attachKey(parent_offset, offset);
    regcache->addKey(key, offset, rkey);

    return offset;
}

long int Registrar_impl::deleteKey(const char *key) {
    int32_t offset;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    if (offset == 0)
        throw Registrar::NotFound();

    regdb->deleteKey(offset);
    regcache->deleteKey(key);

    return 0;
}

long int Registrar_impl::getValueType(const char *key) {
    int32_t value_type;

    isValidKeyName(key);

    try {
        value_type = regcache->getValueType(key);
    } catch (Registrar::NotFound &ex) {
        int32_t offset = regdb->getKeyOffsetByName(key);
        if (offset == 0)
            throw Registrar::NotFound();

        value_type = regdb->getValueType(offset);
    }

    return value_type;
}

short int Registrar_impl::getBooleanValue(const char *key) {
    int32_t offset, data_offset, value_type;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    if (offset == 0)
        throw Registrar::NotFound();

    value_type = regdb->getValueType(offset);

    if (value_type != REG_TYPE_BOOLEAN)
        throw Registrar::InvalidType();

    short int data;
    data = (short int)regdb->getValue(offset);
    
    return (data > 0 ? 1 : 0);
}

long int Registrar_impl::getIntegerValue(const char *key) {
    int32_t offset, data_offset, value_type;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    if (offset == 0)
        throw Registrar::NotFound();

    value_type = regdb->getValueType(offset);

    if (value_type != REG_TYPE_INTEGER)
        throw Registrar::InvalidType();

    long int data;
    data = (long int)regdb->getValue(offset);

    return data;
}

char *Registrar_impl::getStringValue(const char *key) {
    int32_t offset, data_offset, value_type;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    if (offset == 0)
        throw Registrar::NotFound();

    value_type = regdb->getValueType(offset);

    if (value_type != REG_TYPE_STRING)
        throw Registrar::InvalidType();

    char *data;
    data = (char *)regdb->getValue(offset);

    return data;
}

Registrar::binary *Registrar_impl::getBinaryValue(const char *key) {
    int32_t offset, data_offset, value_type;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    if (offset == 0)
        throw Registrar::NotFound();

    value_type = regdb->getValueType(offset);

    if (value_type != REG_TYPE_BINARY)
        throw Registrar::InvalidType();

    Registrar::binary *data = new Registrar::binary;
    data->value = (char *)regdb->getValue(offset);
    data->length = regdb->getValueSize(offset);

    return data;
}

CORBA::Any *Registrar_impl::getValue(const char *key) {
    int32_t offset, data_offset, value_type;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    if (offset == 0)
        throw Registrar::NotFound();

    value_type = regdb->getValueType(offset);

    CORBA::Any *anydata = new CORBA::Any;
    if (value_type == REG_TYPE_BINARY) {
        Registrar::binary data;
        data.value = (char *)regdb->getValue(offset);
        data.length = regdb->getValueSize(offset);
#if 0
        *anydata <<= data;
#endif
    }

    return anydata;
}

long int Registrar_impl::setBooleanValue(const char *key, short int value) {
    int32_t offset, data_offset, size;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }

    size = sizeof(value);
    data_offset = regdb->addValue((char *)value, size);
    regdb->changeKeyValue(offset, data_offset, REG_TYPE_BOOLEAN, size);
    regcache->deleteKey(key);

    return data_offset;
}

long int Registrar_impl::setIntegerValue(const char *key, long int value) {
    int32_t offset, data_offset, size;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }

    size = sizeof(value);
    data_offset = regdb->addValue((char *)value, size);
    regdb->changeKeyValue(offset, data_offset, REG_TYPE_INTEGER, size);
    regcache->deleteKey(key);

    return data_offset;
}

long int Registrar_impl::setStringValue(const char *key, const char *value) {
    int32_t offset, data_offset, size;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }

    size = strlen(value) + 1;

    data_offset = regdb->addValue((char *)value, size);
    regdb->changeKeyValue(offset, data_offset, REG_TYPE_STRING, size);
    regcache->deleteKey(key);

    return data_offset;
}

long int Registrar_impl::setBinaryValue(const char *key, 
                                        const Registrar::binary &value) {
    int32_t offset, data_offset;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }

    data_offset = regdb->addValue(value.value, value.length);
    regdb->changeKeyValue(offset, data_offset, REG_TYPE_BINARY, value.length);
    regcache->deleteKey(key);

    return data_offset;
}

long int Registrar_impl::setValue(const char *key, const CORBA::Any &value) {
    int32_t offset, data_offset;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }

    return data_offset;
}

long int Registrar_impl::deleteValue(const char *key) {
    int32_t offset;

    isValidKeyName(key);

    try {
        offset = regcache->getKeyOffsetByName(key);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(key);
    }
    regdb->changeKeyValue(offset, 0, REG_TYPE_NULL, 0);
    registrar_key rkey = regdb->getKey(offset);
    regcache->deleteKey(key);
    regcache->addKey(key, offset, rkey);    

    return 0;
}

/* This call is better if you always want to get the full keypair since
   it reduces the amount of marshalling involved */

Registrar_impl::keypair *Registrar_impl::getKeyPair(const char *key) {
    Registrar_impl::keypair *kp = new Registrar_impl::keypair;
    registrar_key rkey;
    int32_t offset;

    isValidKeyName(key);

    try {
        rkey = regcache->getKey(key);
    } catch (Registrar::NotFound &ex) {
        int32_t offset = regdb->getKeyOffsetByName(key);
        rkey = regdb->getKey(offset);
        regcache->addKey(key, offset, rkey);
    }
    kp->type = rkey.type;
    kp->ctime = rkey.ctime;
    kp->mtime = rkey.mtime;
    kp->perms.mode = rkey.mode;
    kp->perms.uid = rkey.uid;
    kp->perms.gid = rkey.gid;

    if (rkey.type != REG_TYPE_NULL) {
        try {
            offset = regcache->getKeyOffsetByName(key);
        } catch (Registrar::NotFound &ex) {
            offset = regdb->getKeyOffsetByName(key);
        }
    }
    if (rkey.type == REG_TYPE_STRING) {
        char *value = (char *)regdb->getValue(offset);

        kp->value <<= value;
    } else if (rkey.type == REG_TYPE_INTEGER) {
        long int value = (long int)regdb->getValue(offset);

        kp->value <<= value;
    } else if (rkey.type == REG_TYPE_BOOLEAN) {
        short int value = (short int)regdb->getValue(offset);

        kp->value <<= value;
    } else if (rkey.type == REG_TYPE_BINARY) {
        char *value = (char *)regdb->getValue(offset);

        kp->value <<= value;
    }

    return kp;
}

/* getKeyOffsetByName will return the first entry in the heirarchy if
   the path ends with a '/' */

Registrar_impl::keys *Registrar_impl::enumKeys(const char *key) {
    Registrar_impl::keys *enumkeys = new Registrar_impl::keys;
    char *tkey;
    registrar_key rkey;
    int32_t offset, dlen;

    isValidKeyName(key);

    tkey = CORBA::string_dup(key);

    // Remove any trailing slashes
    dlen = strlen(tkey);
    while (dlen > 1 && tkey[dlen - 1] == '/')
        dlen--;

    if (dlen > 1) {
        char *base;
        base = (char *)malloc(dlen + 1);
        bzero(base, dlen + 1);
        strncpy(base, tkey, dlen);

        try {
            rkey = regcache->getKey(base);
        } catch (Registrar::NotFound &ex) {
            offset = regdb->getKeyOffsetByName(base);
            rkey = regdb->getKey(offset);
            regcache->addKey(base, offset, rkey);
        }
        free(base);
        offset = rkey.subkey_offset;
    } else {
        rkey = regdb->getKey(REGISTRAR_ROOT_OFFSET);
        offset = rkey.subkey_offset;
    }

    int count = 0;
    for (int i = 0; offset != 0; i++) {
        rkey = regdb->getKey(offset);
 
       if (rkey.dtime == 0) {
            (*enumkeys).length((*enumkeys).length() + 1);
            (*enumkeys)[count++] = CORBA::string_dup(rkey.name);
        }
        offset = rkey.key_offset;
    }
    return enumkeys;
}

Registrar::keyaccess Registrar_impl::getSecurityInfo(const char *key) {
    keyaccess keyperms;
    registrar_key rkey;
    char *tkey;
    int32_t offset;

    isValidKeyName(key);

    tkey = CORBA::string_dup(key);
    try {
        rkey = regcache->getKey(tkey);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(tkey);
        rkey = regdb->getKey(offset);
        regcache->addKey(tkey, offset, rkey);
    }
    keyperms.mode = rkey.mode;
    keyperms.uid = rkey.uid;
    keyperms.gid = rkey.gid;    

    return keyperms;
}

long int Registrar_impl::setSecurityInfo(const char *key, 
                                         const keyaccess &perms) {
    registrar_key rkey;
    char *tkey;
    int32_t offset;

    isValidKeyName(key);

    tkey = CORBA::string_dup(key);
    try {
        offset = regcache->getKeyOffsetByName(tkey);
        rkey = regcache->getKey(tkey);
    } catch (Registrar::NotFound &ex) {
        offset = regdb->getKeyOffsetByName(tkey);
        rkey = regdb->getKey(offset);
        regcache->addKey(tkey, offset, rkey);
    }
 
    rkey.mode = perms.mode;
    rkey.uid = perms.uid;
    rkey.gid = perms.gid;    
    regdb->changeKey(&rkey, offset);
    regcache->deleteKey(tkey);

    return 0;
}

long int Registrar_impl::watchKey(const char *key) {
    isValidKeyName(key);

    return 0;
}

long int Registrar_impl::lockKey(const char *key) {
    isValidKeyName(key);

    return 0;
}

long int Registrar_impl::unlockKey(const char *key) {
    isValidKeyName(key);

    return 0;
}

/* basic testing.. make sure everything is aok.

   XXX - need to do character testing 
   XXX - need to do length checking */

long int Registrar_impl::isValidKeyName(const char *key) {
    if (key == NULL)
        throw Registrar::InvalidName();

    return 0;
}

char *Registrar_impl::getVersion(void) {
    char *p = CORBA::string_dup(REGISTRAR_VERSION_NAME);

    return p;
}
