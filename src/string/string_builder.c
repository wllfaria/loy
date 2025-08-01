#include <stdarg.h>
#include <stdio.h>

#include "../collections/vector.h"

#include "string_builder.h"

StringBuilder string_builder_create(void) {
    StringBuilder builder = { .buf = vector_create() };
    return builder;
}

u64 string_builder_write_byte(StringBuilder* builder, char byte) {
    vector_push(&builder->buf, byte);
    return 1;
}

u64 string_builder_write_string(StringBuilder* builder, char* string) {
    StringSlice slice = string_slice_create(string);
    return string_builder_write_slice(builder, slice);
}

u64 string_builder_write_slice(StringBuilder* builder, StringSlice slice) {
    StringChars chars = string_slice_chars(slice);
    u64 len = builder->buf.len;

    for(;;) {
        char next = string_chars_next(&chars);
        if(next == EOF) {
            break;
        }

        string_builder_write_byte(builder, next);
    }

    return builder->buf.len - len;
}

void string_builder_write_format(StringBuilder* builder, const char* fmt, ...) {
    // TODO: tune this number
    char temp[256];

    va_list args;
    va_start(args, fmt);
    u64 len = (u64)vsnprintf(temp, sizeof(temp), fmt, args);
    va_end(args);

    // If output fits, write it
    if(len < (int)sizeof(temp)) {
        string_builder_write_string(builder, temp);
        return;
    }

    // If not, dynamically allocate the required size
    char* dynamic = malloc(len + 1);
    if(!dynamic) {
        return;
    }

    va_start(args, fmt);
    vsnprintf(dynamic, len + 1, fmt, args);
    va_end(args);

    string_builder_write_string(builder, dynamic);
    free(dynamic);
}

void string_builder_indent(StringBuilder* builder, u64 indentation) {
    for(u64 i = 0; i < indentation * PP_SPACES; i++) {
        string_builder_write_byte(builder, ' ');
    }
}

char* string_builder_to_string(StringBuilder* builder) {
    string_builder_write_byte(builder, '\0');
    return (char*)builder->buf.buf;
}

void string_builder_destroy(StringBuilder* builder) {
    vector_destroy(&builder->buf, NULL);
}
