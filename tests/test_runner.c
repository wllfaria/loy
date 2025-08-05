#include <stdio.h>

#include "test_runner.h"
#include "stdarg.h"

const char* RESET = "\x1B[0m";
const char* UNDERSCORE = "\x1B[4m";
const char* GREEN = "\x1B[32m";
const char* PURPLE = "\x1B[35m";
const char* RED = "\x1B[31m";
const char* YELLOW = "\x1B[33m";

typedef struct {
    const char* suite;
    const char* test;
    TestResult  result;
} TestReport;

typedef struct {
    u64 passed;
    u64 failed;
    u64 skipped;
    u64 tests;
} TestRun;

static TestRun run = {
    .failed = 0,
    .tests = 0,
    .passed = 0,
    .skipped = 0,
};

static TestReport curr_test;

TestSuite test_suite_create(const char* name, TestCase cases[], u64 count) {
    TestSuite suite = {
        .name = name,
        .cases = cases,
        .count = count,
    };
    return suite;
}

void test_runner_print_report(TestReport report) {
    switch(report.result) {
    case TEST_PASS:
        run.passed++;
        fprintf(stderr, "   %sPASS%s ", GREEN, RESET);
        break;
    case TEST_FAIL:
        run.failed++;
        fprintf(stderr, "   %sFAIL%s ", RED, RESET);
        break;
    case TEST_SKIP:
        run.skipped++;
        fprintf(stderr, "   %sSKIP%s ", YELLOW, RESET);
        break;
    case TEST_WAIT:
        UNREACHABLE();
    }

    fprintf(stderr, "%s%s->%s%s\n", PURPLE, report.suite, report.test, RESET);
}

void test_runner_print_summary(void) {
    u64 total_tests_run = run.failed + run.passed;
    u64 missing_tests = run.tests - run.failed - run.passed - run.skipped;
    bool has_failed_test = run.failed > 0;

    fprintf(stderr, "────────────\n");
    fprintf(stderr, "%sSummary%s ", has_failed_test ? RED : GREEN, RESET);

    fprintf(stderr, "%ld/%ld tests run: ", total_tests_run, run.tests);

    fprintf(stderr, "%ld %spassed%s, ", run.passed, GREEN, RESET);
    fprintf(stderr, "%ld %sfailed%s, ", run.failed, RED, RESET);
    fprintf(stderr, "%ld %sskipped%s\n", run.skipped, YELLOW, RESET);

    if(missing_tests > 0) {
        fprintf(stderr, "\n");
        fprintf(
            stderr,
            "%sWarning%s: %ld/%ld tests were not run",
            YELLOW, RESET,
            missing_tests,
            run.tests
        );

        if(has_failed_test) {
            fprintf(stderr, " due to %stest failure%s", YELLOW, RESET);
        }

        fprintf(stderr, "\n");
    }
}

void test_runner_run(TestConfig* config) {
    for(u64 i = 0; i < config->count; i++) {
        run.tests += config->suites[i].count;
    }

    printf(
        "%sRunning%s %ld %s across %ld %s\n",
        GREEN,
        RESET,
        run.tests,
        run.tests > 1 ? "tests" : "test",
        config->count,
        config->count > 1 ? "suites" : "suite"
    );

    printf("────────────\n");

    for(u64 i = 0; i < config->count; i++) {
        TestSuite* suite = &config->suites[i];

        for(u64 j = 0; j < suite->count; j++) {
            TestCase* test = &suite->cases[j];
            TestReport report = {
                .suite = suite->name,
                .test = test->name,
                .result = TEST_WAIT,
            };
            curr_test = report;
            curr_test.result = test->subject();
            test_runner_print_report(curr_test);
            // Terminate test run if we encounter a failing test that failed
            // gracefulle
            if(curr_test.result == TEST_FAIL) goto summary;
        }
    }

summary:
    test_runner_print_summary();
}

void test_error_print(const char* file, u64 line, const char* fmt, ...) {
    run.failed++;

    va_list va;
    va_start(va, fmt);

    printf("────────────\n");

    fprintf(stderr, "%s", RED);
    vfprintf(stderr, fmt, va);
    fprintf(stderr, "%s\n", RESET);

    fprintf(
        stderr,
        "    While running test: %s%s%s->%s%s.\n",
        UNDERSCORE,
        PURPLE,
        curr_test.suite,
        curr_test.test,
        RESET
    );

    fprintf(stderr, "    @ %s:%ld\n", file, line);

    u64 missing_tests = run.tests - run.failed - run.passed - run.skipped;
    bool has_run_all_tests = missing_tests == 0;

    if(!has_run_all_tests) {
        fprintf(
            stderr,
            "\n%sCancelling%s due to %stest failure%s.\n",
            RED,
            RESET,
            RED,
            RESET
        );
    }

    va_end(va);

    test_runner_print_summary();
}
