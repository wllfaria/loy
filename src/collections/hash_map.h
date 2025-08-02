#ifndef _HASH_MAP_H
#define _HASH_MAP_H

#include "../defines.h"
#include "vector.h"

typedef struct {
    Vector buckets;
    u64    len;
    u64    bucket_count;
} HashMap;

typedef struct {
    void* key;
    u64   key_len;
    u64   hash;
    void* value;
} HashMapEntry;

HashMap hash_map_create(void);
void hash_map_destroy(HashMap* hash_map, FreeFn free_fn);

void* hash_map_get(HashMap* hash_map, void* key, u64 key_len);
void hash_map_insert(HashMap* hash_map, void* key, u64 key_len, void* value);

#endif
