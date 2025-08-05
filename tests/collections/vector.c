#include <string.h>

#include "vector.h"
#include "../../src/collections/vector.h"
#include "../../src/mem/arena.h"

TestResult test_vector_initialization(void) {
    Allocator allocator = arena_create();
    Vector vec = vector_create(&allocator);
    test_assert(vec.buf == NULL);
    test_assert(vec.cap == 0);
    test_assert(vec.len == 0);
    arena_destroy(allocator.ctx);
    return TEST_PASS;
}

TestResult test_vector_growth(void) {
    Allocator allocator = arena_create();
    Vector vec = vector_create(&allocator);
    test_assert(vec.cap == 0);
    vector_push(&vec, 0ull);
    test_assert(vec.cap == 4);

    for(u64 i = 1; i < 100; i++) {
        vector_push(&vec, i);
    }

    for(u64 i = 0; i < 100; i++) {
        u64* val = vector_get(&vec, i);
        test_assert(*val == i);
    }

    test_assert(vec.len == 100);
    vector_destroy(&vec);
    arena_destroy(allocator.ctx);
    return TEST_PASS;
}

TestResult test_vec_push_ptr(void) {
    Allocator allocator = arena_create();
    Vector vec = vector_create(&allocator);
    const char* words[] = { "alpha", "beta", "gamma" };

    for(u64 i = 0; i < 3; i++) {
        vector_push_ptr(&vec, words[i]);
    }

    for(u64 i = 0; i < 3; i++) {
        char* val = vector_get(&vec, i);
        test_assert(strcmp(val, words[i]) == 0);
    }

    vector_destroy(&vec);
    arena_destroy(allocator.ctx);
    return TEST_PASS;
}

TestResult test_vector_iterator(void) {
    Allocator allocator = arena_create();
    Vector vec = vector_create(&allocator);
    for(u64 i = 0; i < 5; i++) {
        vector_push(&vec, i);
    }

    VectorIter iter = vector_iter(&vec);
    for(u64 i = 0; i < 5; i++) {
        u64* peeked = vector_iter_peek(&iter);
        test_assert(peeked != NULL);
        test_assert(*peeked == i);

        u64* val = vector_iter_next(&iter);
        test_assert(val != NULL);
        test_assert(*val == i);
    }

    test_assert(vector_iter_peek(&iter) == NULL);
    test_assert(vector_iter_next(&iter) == NULL);

    vector_destroy(&vec);
    arena_destroy(allocator.ctx);
    return TEST_PASS;
}

TestResult test_vector_push_and_get(void) {
    Allocator allocator = arena_create();
    Vector vec = vector_create(&allocator);

    for(u64 i = 0; i < 10; i++) {
        vector_push(&vec, i);
        u64* val = vector_get(&vec, i);
        test_assert(val != NULL);
        test_assert(*val == i);
    }

    test_assert(vec.len == 10);
    vector_destroy(&vec);
    arena_destroy(allocator.ctx);
    return TEST_PASS;
}

TestCase vector_test_cases[] = {
    {
        .name = "test_vector_initialization",
        .subject = test_vector_initialization
    },
    {
        .name = "test_vector_growth",
        .subject = test_vector_growth
    },
    {
        .name = "test_vec_push_ptr",
        .subject = test_vec_push_ptr
    },
    {
        .name = "test_vector_iterator",
        .subject = test_vector_iterator
    },
    {
        .name = "test_vector_push_and_get",
        .subject = test_vector_push_and_get
    },
};

TestSuite const vector_test_suite = {
    .name = "vector",
    .cases = vector_test_cases,
    .count = sizeof(vector_test_cases) / sizeof(vector_test_cases[0]),
};
