#include <assert.h>
#include <string.h>

#include "hash_map.h"
#include "linked_list.h"

#define MIN_MAP_BUCKET_SIZE 16
#define MAP_GROW_THRESHOLD  2.5

u64 hash_map_idx_from_key_hash(u64 bucket_count, u64 key_hash) {
    return key_hash & (bucket_count - 1);
}

static u64 hash_map_figure_bucket_count(HashMap* hash_map) {
    // if uninitialized, min bucket count is MIN_MAP_BUCKET_SIZE
    if(hash_map->bucket_count == 0) return MIN_MAP_BUCKET_SIZE;

    // if the ratio is over MAP_GROW_THRESHOLD we double the bucket count
    f32 map_fill_ratio = (f32)hash_map->len / (f32)hash_map->bucket_count;
    if(map_fill_ratio > MAP_GROW_THRESHOLD) return hash_map->bucket_count * 2;

    // else we keep the same size
    return hash_map->bucket_count;
}

static void hash_map_initialize(HashMap* hash_map) {
    assert(hash_map->bucket_count == 0);
    u64 bucket_count = hash_map_figure_bucket_count(hash_map);

    hash_map->bucket_count = bucket_count;
    hash_map->buckets      = vector_create_with_capacity(bucket_count);
    hash_map->len          = 0; // this is already set, but doesn't hurt

    for(u8 i = 0; i < bucket_count; i++) {
        vector_push(&hash_map->buckets, linked_list_create());
    }
}

static void hash_map_grow(HashMap* hash_map, u64 bucket_count) {
    Vector new_buckets = vector_create_with_capacity(bucket_count);
    for(u64 i = 0; i < bucket_count; i++) {
        vector_push(&new_buckets, linked_list_create());
    }

    // Go over every bucket we have, so we can copy pointers to the new buckets
    for(u64 i = 0; i < hash_map->bucket_count; i++) {
        LinkedList* bucket = (LinkedList*)vector_get(&hash_map->buckets, i);

        LinkedListIter iter = linked_list_iter_create(bucket);
        while(linked_list_iter_peek(&iter) != NULL) {
            LinkedListItem* value =
                (LinkedListItem*)linked_list_iter_next(&iter);

            HashMapEntry* entry    = (HashMapEntry*)value->value;
            LinkedList* new_bucket = (LinkedList*)vector_get(
                &new_buckets,
                hash_map_idx_from_key_hash(bucket_count, entry->hash)
            );
            linked_list_insert_tail(new_bucket, entry);
        }
    }

    for(u64 i = 0; i < hash_map->bucket_count; i++) {
        LinkedList* bucket = vector_get(&hash_map->buckets, i);
        linked_list_destroy(bucket, NULL); // Don't free entries, just the structure
    }

    vector_destroy(&hash_map->buckets, NULL);
    hash_map->buckets      = new_buckets;
    hash_map->bucket_count = bucket_count;
}

static HashMapEntry* hash_map_entry_create(
    void* key,
    u64 key_len,
    u64 key_hash,
    void* value
) {
    HashMapEntry* entry = malloc(sizeof(HashMapEntry));

    // Copy the key so we own the pointer data
    entry->key = malloc(key_len);
    memcpy(entry->key, key, key_len);
    entry->key_len = key_len;
    entry->hash    = key_hash;
    entry->value   = value;
    return entry;
}

// FNV-1a implementation for 64 bit data
static u64 hash_buffer(const void* buffer, u64 len) {
    u64 hash       = 14695981039346656037ull;
    u64 prime      = 1099511628211ull;
    const u8* data = (u8*)buffer;

    for(u64 i = 0; i < len; i++) {
        hash ^= data[i];
        hash *= prime;
    }

    return hash;
}

HashMap hash_map_create(void) {
    HashMap hash_map = {
        .buckets      = vector_create(),
        .len          = 0,
        .bucket_count = 0,
    };
    return hash_map;
}

void hash_map_destroy(HashMap* hash_map, FreeFn free_fn) {
    for(u64 i = 0; i < hash_map->bucket_count; i++) {
        LinkedList* bucket  = (LinkedList*)vector_get(&hash_map->buckets, i);
        LinkedListIter iter = linked_list_iter_create(bucket);

        while(linked_list_iter_peek(&iter) != NULL) {
            LinkedListItem* val = (LinkedListItem*)linked_list_iter_next(&iter);
            HashMapEntry* entry = (HashMapEntry*)val->value;
            free(entry->key);
            free(entry);
        }

        linked_list_destroy(bucket, free_fn);
    }

    vector_destroy(&hash_map->buckets, NULL);
}

void* hash_map_get(HashMap* hash_map, void* key, u64 key_len) {
    assert(hash_map != NULL);
    u64 key_hash = hash_buffer(key, key_len);

    u64 idx = hash_map_idx_from_key_hash(hash_map->bucket_count, key_hash);

    LinkedList* bucket  = vector_get(&hash_map->buckets, idx);
    LinkedListIter iter = linked_list_iter_create(bucket);
    while(linked_list_iter_peek(&iter) != NULL) {
        LinkedListItem* item = linked_list_iter_next(&iter);
        HashMapEntry* entry  = (HashMapEntry*)item->value;

        // check hash value and length before memcmp as those are less expensive
        bool has_same_hash = entry->hash == key_hash;
        bool has_same_len  = entry->key_len == key_len;
        if(
            has_same_hash &&
            has_same_len &&
            memcmp(entry->key, key, key_len) == 0
        ) {
            return entry->value;
        }
    }

    return NULL;
}

void hash_map_insert(HashMap* hash_map, void* key, u64 key_len, void* value) {
    assert(hash_map != NULL);
    if(hash_map->bucket_count == 0) hash_map_initialize(hash_map);

    u64 key_hash = hash_buffer(key, key_len);
    u64 idx      = hash_map_idx_from_key_hash(hash_map->bucket_count, key_hash);

    LinkedList* bucket  = (LinkedList*)vector_get(&hash_map->buckets, idx);
    HashMapEntry* entry = hash_map_entry_create(key, key_len, key_hash, value);

    linked_list_insert_tail(bucket, entry);
    hash_map->len++;

    u64 min_bucket_count = hash_map_figure_bucket_count(hash_map);
    if(min_bucket_count > hash_map->bucket_count) {
        hash_map_grow(hash_map, min_bucket_count);
    }
}
