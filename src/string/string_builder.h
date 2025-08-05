#ifndef _STR_BUILDER_H
#define _STR_BUILDER_H

#include "../collections/vector.h"
#include "string_slice.h"
#include "../mem/allocator.h"

typedef struct {
    Vector     buf;
    Allocator* allocator;
} StringBuilder;

StringBuilder string_builder_create(Allocator* allocator);
u64 string_builder_write_byte(StringBuilder* builder, char byte);
u64 string_builder_write_slice(StringBuilder* builder, StringSlice slice);
void string_builder_write_format(StringBuilder* builder, const char* fmt, ...);
u64 string_builder_write_string(StringBuilder* builder, char* string);
void string_builder_indent(StringBuilder* builder, u64 indentation);
void string_builder_destroy(StringBuilder* builder);
char* string_builder_to_string(StringBuilder* builder);

#endif
