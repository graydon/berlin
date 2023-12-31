#ifndef __DCACHE_HH__
#define __DCACHE_HH__ 1

#include <hash_map.h>
#include <hashtable.h>
#include "support/registrar/Db.hh"

class eq {
public:
    bool operator() (const char *s1, const char *s2) const {
        return strcmp(s1, s2) == 0;
    }
};

typedef struct dcache_key {
    int32_t offset;
    registrar_key key;
};

class Dcache {
private:
    hash_map<const char *, dcache_key, hash<const char *>, eq> key_cache;

public:
    ~Dcache() {}

    Dcache();

    registrar_key getKey(const char *key);

    void addKey(const char *key, int32_t offset, registrar_key rkey);

    void deleteKey(const char *key);

    int32_t getValueType(const char *key);

    int32_t getValueSize(const char *key);

    int32_t getKeyOffsetByName(const char *key);
};

#endif /* __DCACHE_HH__ */
