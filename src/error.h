#ifndef _ERROR_H
#define _ERROR_H

#include "collections/vector.h"
#include "defines.h"
#include "string/string_slice.h"
#include "mem/allocator.h"

typedef struct {
    u64 line_start;
    u64 line_end;
    u64 column_start;
    u64 column_end;
} Location;

typedef struct {
    u64 start;
    u64 end;
} ByteOffset;

typedef enum {
    SPAN_WARN,
    SPAN_ERROR,
} SpanLevel;

typedef struct {
    ByteOffset  offset;
    const char* label;
    StringSlice file;
    SpanLevel   level;
    const char* info;
} Span;

typedef struct {
    Vector spans;
} Report;

typedef struct {
    const char* path;
    const char* relative_path;
    StringSlice content;
} CompilationUnit;

typedef struct {
    Report          report;
    CompilationUnit unit;
} CompileContext;

ByteOffset byte_offset_merge(ByteOffset a, ByteOffset b);

LoyResult error_report_push_span(Report* report, Span span);
LoyResult error_report_print(Report* report);
Location location_from_byte_offset(StringSlice file, ByteOffset offset);

char* fmt_location(Allocator* allocator, Location loc, u64 indentation);
char* fmt_byte_offset(
    Allocator* allocator,
    ByteOffset byte_offset,
    u64 indentation
);

#endif
