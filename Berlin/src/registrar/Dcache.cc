#include <iostream.h>
#include <hash_map.h>
#include <hashtable.h>
#include <syslog.h>

#include "support/registrar/Dcache.hh"
#include "support/registrar/Registrar.hh"
#include "support/registrar/RegistrarDefs.hh"
#include "support/registrar/Db.hh"
#include "support/registrar/Db.hh"

Dcache::Dcache() {}

registrar_key Dcache::getKey(const char *key) {
    if (key_cache.find(key) == key_cache.end())
        throw Registrar::NotFound();

    dcache_key dkey = key_cache[key];

    return dkey.key;
}

/* Add a key to the cache. Note that the value passed is not a pointer.
   This keeps us from having to copy the entire structure in the function */

void Dcache::addKey(const char *key, int32_t offset, registrar_key rkey) {
    dcache_key dkey;

    dkey.offset = offset;
    dkey.key = rkey;

    /* we must duplicate the key to a local variable to keep the calling
       function from from corrupting it */

    char *keyname = strdup(key);
    key_cache[keyname] = dkey;
}

void Dcache::deleteKey(const char *key) {
    if (key_cache.find(key) != key_cache.end()) {
        key_cache.erase(key);    
    }
}

int32_t Dcache::getValueType(const char *key) {
    if (key_cache.find(key) == key_cache.end())
        throw Registrar::NotFound();

    dcache_key dkey = key_cache[key];

    return dkey.key.type;
}

int32_t Dcache::getValueSize(const char *key) {
    if (key_cache.find(key) == key_cache.end())
        throw Registrar::NotFound();

    dcache_key dkey = key_cache[key];

    return dkey.key.value_size;
}

int32_t Dcache::getKeyOffsetByName(const char *key) {
    if (key_cache.find(key) == key_cache.end()) {
        throw Registrar::NotFound();
    }
    dcache_key dkey = key_cache[key];

    return dkey.offset;
}
