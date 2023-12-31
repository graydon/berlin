#ifndef __DB_HH__
#define __DB_HH__ 1

typedef struct registrar_header {
    int32_t magic;
    int32_t version;
    int32_t ctime;
    int32_t mtime;
    int8_t clean;
    int8_t extra[1007];
};

typedef struct registrar_key {
    char name[16];

    int8_t mode;
    int16_t uid;
    int16_t gid;
    int32_t ctime;
    int32_t mtime;
    int32_t dtime;
    int32_t type;
    int32_t value_size;
    int32_t value_offset;
    int32_t key_offset;
    int32_t subkey_offset;
};

class Db {
private:
    int treefd, datafd;
public:
    ~Db() {}

    Db();

    void initDb(void);

    void closeDb(void);

    int32_t addKey(registrar_key *rkey);

    int32_t changeKey(registrar_key *rkey, int32_t key_offset);

    registrar_key getKey(int32_t key_offset);

    int32_t addValue(const char *data, int32_t size);

    int32_t changeKeyValue(int32_t key_offset, int32_t data_offset,
                           int32_t data_type, int32_t data_size);

    char *getValue(int32_t key_offset);

    int32_t getValueType(int32_t key_offset);

    int32_t getValueSize(int32_t key_offset);

    int32_t deleteKey(int32_t key_offset);

    int32_t attachKey(int32_t parent_offset, int32_t key_offset);

    int32_t readOffset(int fd, int32_t offset, char *data, int32_t length);

    int32_t writeOffset(int fd, int32_t offset, const char *data, 
                        int32_t length);

    int32_t getKeyOffsetByName(const char *key);

    int32_t findSubKey(int32_t parent_offset, const char *key);
};

#endif /* __DB_HH__ */
