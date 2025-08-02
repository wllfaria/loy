#include "collections/hash_map.h"
#include "test_runner.h"
#include "collections/linked_list.h"

int main(void) {
    TestSuite suites[] = {
        linked_list_test_suite,
        hash_map_test_suite,
    };

    TestConfig config = {
        .suites = suites,
        .count  = sizeof(suites) / sizeof(suites[0]),
    };

    test_runner_run(&config);

    return 0;
}
