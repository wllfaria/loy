#ifndef _TEST_RUNNER_H
#define _TEST_RUNNER_H

#include "../src/defines.h"

typedef enum { TEST_PASS, TEST_FAIL, TEST_SKIP, TEST_WAIT } TestResult;
typedef TestResult (*TestFn)(void);

typedef struct {
    const char* name;
    TestFn      subject;
} TestCase;

typedef struct {
    const char* name;
    TestCase*   cases;
    u64         count;
} TestSuite;

typedef struct {
    TestSuite* suites;
    u64        count;
} TestConfig;

TestSuite test_suite_create(const char* name, TestCase* cases, u64 count);

void test_runner_run(TestConfig* config);

void test_error_print(const char* file, u64 line, const char* fmt, ...);

#define test_error_fmt(fmt, ...) \
        test_error_print(__FILE__, __LINE__, fmt, __VA_ARGS__)

#define test_error(msg)            \
        test_error_fmt("%s", msg); \
        exit(EXIT_SUCCESS);

#define test_assert(expr)                           \
        do                                          \
        if(!(expr)) {                               \
            test_error("Assertion failed: " #expr); \
        }                                           \
        while(0)

#endif
