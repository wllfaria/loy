#include "../test_runner.h"
#include "hash_map.h"
#include "../../src/collections/hash_map.h"

TestResult test_hash_map_initialization(void) {
    HashMap hash_map = hash_map_create();
    hash_map_insert(&hash_map, "first", 5, "first");

    test_assert(hash_map.bucket_count == 16);
    test_assert(hash_map.len == 1);

    hash_map_destroy(&hash_map, NULL);
    return TEST_PASS;
}

TestResult test_hash_map_growth(void) {
    HashMap hash_map = hash_map_create();
    test_assert(hash_map.bucket_count == 0); // assert no allocations without a insert

    // Default hash_map size is 16, the formula for growth factor is:
    //
    // (map_items / bucket_count) > 2.5
    //
    // That is, 41 is the threshold as `41 / 16 = 2.5625 (> 2.5)`

    // Insert 40 elements -> no growth yet
    static u8 values[40];
    for(u8 i = 0; i < 40; i++) {
        values[i] = i;
        hash_map_insert(&hash_map, &i, 1, &values[i]);
    }
    test_assert(hash_map.bucket_count == 16);

    // Insert one more -> triggers growth to 32
    u8 k = 40;
    hash_map_insert(&hash_map, &k, 1, &k);

    test_assert(hash_map.len == 41);
    test_assert(hash_map.bucket_count == 32);

    // All values should still be retrievable
    for(u8 i = 1; i <= 40; i++) {
        void* val = hash_map_get(&hash_map, &i, 1);
        test_assert(val != NULL);
        test_assert(*(u8*)val == i);
    }

    hash_map_destroy(&hash_map, NULL);
    return TEST_PASS;
}

TestCase hash_map_test_cases[] = {
    {
        .name    = "test_hash_map_initialization",
        .subject = test_hash_map_initialization,
    },
    {
        .name    = "test_hash_map_growth",
        .subject = test_hash_map_growth,
    },
};

TestSuite const hash_map_test_suite = {
    .name  = "hash_map",
    .cases = hash_map_test_cases,
    .count = sizeof(hash_map_test_cases) / sizeof(hash_map_test_cases[0]),
};
