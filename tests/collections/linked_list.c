#include "linked_list.h"
#include "../../src/collections/linked_list.h"

static u64 free_called = 0;

void mock_free(void* ptr) {
    (void)ptr; // silence unused warning
    free_called++;
}

TestResult test_destroy(void) {
    LinkedList list = linked_list_create();

    u64 a = 1;
    u64 b = 2;
    u64 c = 3;

    linked_list_insert_tail(&list, &a);
    linked_list_insert_tail(&list, &b);
    linked_list_insert_tail(&list, &c);
    test_assert(list.len == 3);

    free_called = 0;
    linked_list_destroy(&list, mock_free);

    test_assert(free_called == 3);
    test_assert(list.len == 0);
    test_assert(list.head == NULL);
    test_assert(list.tail == NULL);

    return TEST_PASS;
}

TestResult test_insert_idx(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;
    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    linked_list_insert_tail(&list, &third);
    test_assert(list.len == 3);

    u64 value = 4;
    linked_list_insert_idx(&list, 2, &value);
    test_assert(*(u64*)list.head->value == 1);
    test_assert(*(u64*)list.head->next->value == 2);
    test_assert(*(u64*)list.head->next->next->value == 4);
    test_assert(*(u64*)list.tail->value == 3);
    test_assert(list.len == 4);

    return TEST_PASS;
}

TestResult test_insert_head(void) {
    LinkedList list = linked_list_create();

    u64 first = 1;
    linked_list_insert_head(&list, &first);
    test_assert(*(u64*)list.head->value == 1);
    test_assert(*(u64*)list.tail->value == 1);
    test_assert(list.len == 1);
    test_assert(list.head == list.tail);

    u64 second = 2;
    linked_list_insert_head(&list, &second);
    test_assert(*(u64*)list.head->value == 2);
    test_assert(*(u64*)list.tail->value == 1);
    test_assert(list.len == 2);
    test_assert(list.head != list.tail);

    return TEST_PASS;
}

TestResult test_insert_tail(void) {
    LinkedList list = linked_list_create();

    u64 first = 1;
    linked_list_insert_tail(&list, &first);
    test_assert(*(u64*)list.head->value == 1);
    test_assert(*(u64*)list.tail->value == 1);
    test_assert(list.len == 1);
    test_assert(list.head == list.tail);

    u64 second = 2;
    linked_list_insert_tail(&list, &second);
    test_assert(*(u64*)list.head->value == 1);
    test_assert(*(u64*)list.tail->value == 2);
    test_assert(list.len == 2);
    test_assert(list.head != list.tail);

    return TEST_PASS;
}

TestResult test_get_idx_existing(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;

    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    LinkedListItem* expected = linked_list_insert_tail(&list, &third);

    LinkedListItem* result = linked_list_get_idx(&list, 2);

    test_assert(expected == result);
    test_assert(result->next == NULL);
    test_assert(*(u64*)result->value == 3);

    return TEST_PASS;
}

TestResult test_get_idx_null(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;

    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    linked_list_insert_tail(&list, &third);

    LinkedListItem* result = linked_list_get_idx(&list, 3);

    test_assert(result == NULL);

    return TEST_PASS;
}

TestResult test_remove_idx_first(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;

    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    linked_list_insert_tail(&list, &third);

    LinkedListItem* result = linked_list_remove_idx(&list, 0);

    test_assert(result != NULL);
    test_assert(result->next == NULL);
    test_assert(*(u64*)result->value == 1);
    test_assert(*(u64*)list.head->value == 2);
    test_assert(*(u64*)list.head->next->value == 3);
    test_assert(*(u64*)list.tail->value == 3);
    test_assert(list.len == 2);

    return TEST_PASS;
}

TestResult test_remove_idx_middle(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;

    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    linked_list_insert_tail(&list, &third);

    LinkedListItem* result = linked_list_remove_idx(&list, 1);

    test_assert(result != NULL);
    test_assert(result->next == NULL);
    test_assert(*(u64*)result->value == 2);
    test_assert(*(u64*)list.head->value == 1);
    test_assert(*(u64*)list.head->next->value == 3);
    test_assert(*(u64*)list.tail->value == 3);
    test_assert(list.len == 2);

    return TEST_PASS;
}

TestResult test_remove_head(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;

    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    linked_list_insert_tail(&list, &third);

    LinkedListItem* result = linked_list_remove_head(&list);

    test_assert(result != NULL);
    test_assert(result->next == NULL);
    test_assert(*(u64*)result->value == 1);
    test_assert(*(u64*)list.head->value == 2);
    test_assert(*(u64*)list.head->next->value == 3);
    test_assert(*(u64*)list.tail->value == 3);
    test_assert(list.len == 2);

    return TEST_PASS;
}

TestResult test_remove_tail(void) {
    LinkedList list = linked_list_create();
    u64 first       = 1;
    u64 second      = 2;
    u64 third       = 3;

    linked_list_insert_tail(&list, &first);
    linked_list_insert_tail(&list, &second);
    linked_list_insert_tail(&list, &third);

    LinkedListItem* result = linked_list_remove_tail(&list);

    test_assert(result != NULL);
    test_assert(result->next == NULL);
    test_assert(*(u64*)result->value == 3);
    test_assert(*(u64*)list.head->value == 1);
    test_assert(*(u64*)list.head->next->value == 2);
    test_assert(*(u64*)list.tail->value == 2);
    test_assert(list.len == 2);

    return TEST_PASS;
}

TestCase linked_list_test_cases[] = {
    { .name = "test_destroy",           .subject = test_destroy           },

    { .name = "test_insert_idx",        .subject = test_insert_idx        },
    { .name = "test_insert_head",       .subject = test_insert_head       },
    { .name = "test_insert_tail",       .subject = test_insert_tail       },

    { .name = "test_get_idx_existing",  .subject = test_get_idx_existing  },
    { .name = "test_get_idx_null",      .subject = test_get_idx_null      },

    { .name = "test_remove_idx_first",  .subject = test_remove_idx_first  },
    { .name = "test_remove_idx_middle", .subject = test_remove_idx_middle },
    { .name = "test_remove_head",       .subject = test_remove_head       },
    { .name = "test_remove_tail",       .subject = test_remove_tail       },
};

TestSuite const linked_list_test_suite = {
    .name  = "linked_list",
    .cases = linked_list_test_cases,
    .count = sizeof(linked_list_test_cases) / sizeof(linked_list_test_cases[0]),
};
