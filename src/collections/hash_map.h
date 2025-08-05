#ifndef _HASH_MAP_H
#define _HASH_MAP_H

#include "../defines.h"
#include "../mem/allocator.h"
#include "vector.h"

typedef struct {
    Vector     buckets;
    u64        len;
    u64        bucket_count;
    Allocator* allocator;
} HashMap;

typedef struct {
    void* key;
    u64   key_len;
    u64   hash;
    void* value;
} HashMapEntry;

HashMap hash_map_create(Allocator* allocator);
void hash_map_destroy(HashMap* hash_map);

void* hash_map_get(HashMap* hash_map, void* key, u64 key_len);
void hash_map_insert(HashMap* hash_map, void* key, u64 key_len, void* value);

typedef char* (*EntryFmt)(Allocator*, HashMapEntry*, u64);
void hash_map_inspect(
    Allocator* allocator,
    HashMap* hash_map,
    EntryFmt entry_fmt
);

#endif
