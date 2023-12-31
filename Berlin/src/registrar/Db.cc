#include <time.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <iostream.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/types.h>

#include "support/registrar/Db.hh"
#include "support/registrar/Dcache.hh"
#include "support/registrar/Registrar.hh"
#include "support/registrar/RegistrarDefs.hh"

Db::Db() {
    return;
}

void Db::initDb(void) {
    int createroot = 0;

    treefd = open(REGISTRAR_RT_PATH, O_RDWR | O_SYNC, 0600);
    if (treefd == -1 && errno == ENOENT) {
        registrar_header rhead;

        rhead.magic = REGISTRAR_TREE_MAGIC;
        rhead.version = REGISTRAR_VERSION_CODE;
        rhead.ctime = time(0);
        rhead.mtime = time(0);
        rhead.clean = 0;
        bzero(rhead.extra, sizeof(rhead.extra));

        treefd = open(REGISTRAR_RT_PATH, O_RDWR | O_CREAT | O_SYNC, 0600);
        if (treefd == -1)
            throw Registrar::InternalError("Unable to open user.rt");

        writeOffset(treefd, 0, (char *)&rhead, sizeof(rhead));
        unlink("/tmp/user.rd");
        createroot = 1;
    } else if (treefd == -1)
        throw Registrar::InternalError("Unable to open user.rt");

    datafd = open(REGISTRAR_RD_PATH, O_RDWR | O_SYNC, 0600);
    if (datafd == -1 && errno == ENOENT) {
        registrar_header rhead;

        rhead.magic = REGISTRAR_DATA_MAGIC;
        rhead.version = REGISTRAR_VERSION_CODE;
        rhead.ctime = time(0);
        rhead.mtime = time(0);
        rhead.clean = 0;
        bzero(rhead.extra, sizeof(rhead.extra));

        datafd = open(REGISTRAR_RD_PATH, O_RDWR | O_CREAT | O_SYNC, 0600);
        if (datafd == -1)
            throw Registrar::InternalError("Unable to open user.rd");

        writeOffset(datafd, 0, (char *)&rhead, sizeof(rhead));
        createroot = 1;
    } else if (datafd == -1)
        throw Registrar::InternalError("Unable to open user.rd");


    if (createroot) {
        registrar_key rkey;
        rkey.mode = 0;
        rkey.uid = 0;             /* "Root" or Berlin System Owner */
        rkey.gid = 0;
        rkey.ctime = time(0);
        rkey.mtime = time(0);
        rkey.dtime = 0;           /* since unsigned 0 == never */
        rkey.type = REG_TYPE_NULL;
        rkey.value_size = 0;
        rkey.value_offset = 0;    /* when there is no value, offset = 0 */
        rkey.key_offset = 0;      /* there are no keys past this */
        rkey.subkey_offset = 0;   /* ditto for subkeys */
        bzero(rkey.name, sizeof(rkey.name));

        int32_t offset, parent_offset;

        // The root inode is what all other inodes descend from
        strcpy(rkey.name, "ROOT");
        parent_offset = addKey(&rkey);
        
        strcpy(rkey.name, "tmp");
        offset = addKey(&rkey);
        attachKey(parent_offset, offset);

        strcpy(rkey.name, "user");
        offset = addKey(&rkey);
        attachKey(parent_offset, offset);

        strcpy(rkey.name, "host");
        offset = addKey(&rkey);
        attachKey(parent_offset, offset);

        strcpy(rkey.name, "system");
        offset = addKey(&rkey);
        attachKey(parent_offset, offset);
    }


    return;
}

void Db::closeDb(void) {
    if (treefd != -1) {
        close(treefd);
    }
    if (datafd != -1) {
        close(datafd);
    }
}

/* This function simply finds the next available free area in the database
   file and writes the key header to it. It then returns the offset of the
   header so it may be attached to another key. 

   XXX - Ok, right now it just appends to the treefile.. */

int32_t Db::addKey(registrar_key *rkey) {
    int32_t offset;

    offset = (int32_t)lseek(treefd, (off_t)1, SEEK_END);
    writeOffset(treefd, offset, (char *)rkey, sizeof(registrar_key));

    return offset;
}

/* Changes a key's structure at a given offset

   XXX - Needs error checking */

int32_t Db::changeKey(registrar_key *rkey, int32_t key_offset) {
    return writeOffset(treefd, key_offset, (char *)rkey, sizeof(registrar_key));
}

/* Returns an registrar_key structure for a given offset.

   XXX - Needs error checking */

registrar_key Db::getKey(int32_t key_offset) {
    registrar_key rkey;

    readOffset(treefd, key_offset, (char *)&rkey, sizeof(registrar_key));

    return rkey;
}

int32_t Db::addValue(const char *data, int32_t size) {
    int32_t offset;

    offset = (int32_t)lseek(datafd, (off_t)1, SEEK_END);
    writeOffset(datafd, offset, data, size);

    return offset;
}

/* This changes the offset in the registrar key structure for a particular
   key to point to a new peice of data. Note there is no deletion function
   for data, it will automatically be cleaned up by the compress
   function.

   Set data offset to 0 to delete it completely.
 */

int32_t Db::changeKeyValue(int32_t key_offset, int32_t data_offset,
                           int32_t data_type, int32_t data_size) {
    registrar_key rkey;

    rkey = getKey(key_offset);
    rkey.value_offset = data_offset;
    rkey.value_size = data_size;
    rkey.type = data_type;
    return writeOffset(treefd, key_offset, (char *)&rkey, sizeof(registrar_key));

}

char *Db::getValue(int32_t key_offset) {
    registrar_key rkey;
    char *value;

    rkey = getKey(key_offset);

    if (rkey.value_offset == 0 || rkey.value_size == 0)
        return (char *)NULL;

    value = (char *)malloc(rkey.value_size);
    bzero(value, rkey.value_size);
    readOffset(datafd, rkey.value_offset, value, rkey.value_size);

    return value;
}

int32_t Db::getValueType(int32_t key_offset) {
    registrar_key rkey;

    rkey = getKey(key_offset);

    return rkey.type;
}

int32_t Db::getValueSize(int32_t key_offset) {
    registrar_key rkey;

    rkey = getKey(key_offset);

    return rkey.value_size;
}


/* mark a key deleted.. we don't actually remove anything
   however since the compress routine should remove the key automatically
   during the next pass */

int32_t Db::deleteKey(int32_t key_offset) {
    registrar_key rkey;

    rkey = getKey(key_offset);
    rkey.dtime = time(0);
    writeOffset(treefd, key_offset, (char *)&rkey, sizeof(registrar_key));
}

/* This function takes the offset of a key written to the database
   and attaches it to it's parent offset to create a heirarchial style
   database. */

int32_t Db::attachKey(int32_t parent_offset, int32_t key_offset) {
    int32_t offset;
    registrar_key rkey;

    rkey = getKey(parent_offset);

    offset = rkey.subkey_offset;
    if (offset != 0) {
        rkey = getKey(offset);
    } else {
        rkey.subkey_offset = key_offset;
        writeOffset(treefd, parent_offset, (char *)&rkey, sizeof(registrar_key));
        return 0;
    }

    // we need to traverse this level of the heirarchy
    while (rkey.key_offset != 0) {
        offset = rkey.key_offset;
        rkey = getKey(offset);
    }

    rkey.key_offset = key_offset;
    writeOffset(treefd, offset, (char *)&rkey, sizeof(registrar_key));
    return offset;
}

int32_t Db::readOffset(int fd, int32_t offset, char *data, int32_t length) {
    int rc;

    if (lseek(fd, offset, SEEK_SET) == -1)
        throw Registrar::InternalError("Unable to seek");

    do {
        rc = read(fd, data, length);
    } while (rc == -1 && (errno == EINTR || errno == EAGAIN));

    return (rc == -1 ? errno : 0);
}

int32_t Db::writeOffset(int fd, int32_t offset, const char *data, 
                        int32_t length) {
    int rc;

    if (lseek(fd, offset, SEEK_SET) == -1)
        throw Registrar::InternalError("Unable to seek");

    do {
        rc = write(fd, data, length);
    } while (rc == -1 && (errno == EINTR || errno == EAGAIN));

    return (rc == -1 ? errno : 0);
}

/* This function will return the offset of a key. It takes an argument
   such as '/minute/maid/orange/soda', parses each key out and traverses
   the database to find the key 'soda' which it then returns the offset
   for.

   XXX - This needs caching.
 */

int32_t Db::getKeyOffsetByName(const char *key) {
    char *tkey, *keyname;
    int32_t offset = REGISTRAR_ROOT_OFFSET;

    keyname = strdup(key);


    /* strip the leading / off the key */
    while (*keyname == '/')
        keyname++;

    tkey = keyname;
    while (*tkey) {
        tkey++;
        if (*tkey == '/') {
            *tkey = '\0'; 

            while (*tkey == '/')
                tkey++;
            tkey++;

            offset = findSubKey(offset, keyname);
            keyname = tkey;
        } else if (*tkey == '\0') {
            offset = findSubKey(offset, keyname);
        }
    }

    registrar_key rkey = getKey(offset);
    if (rkey.dtime != 0)
        return 0;

    return offset;
}

/* This finds a subkey for a particular parent key. This will only
   return a result if it matches the correct name and doesn't have 
   dtime set. */

int32_t Db::findSubKey(int32_t parent_offset, const char *key) {
    registrar_key rkey;
    int32_t offset = parent_offset;

    rkey = getKey(parent_offset);

    offset = rkey.subkey_offset;
    if (offset == 0)
        return 0;

    rkey = getKey(offset);
    if (!strcmp(rkey.name, key) && rkey.dtime == 0)
        return offset;

    // we need to traverse this level of the heirarchy
    while (rkey.key_offset != 0) {
        offset = rkey.key_offset;
        rkey = getKey(offset);

        if (!strcmp(rkey.name, key) && rkey.dtime == 0)
            return offset;
    }

    return 0;
}
