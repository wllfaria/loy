#include <stdio.h>

#include "defines.h"
#include "error.h"
#include "string/string_builder.h"

Location location_from_byte_offset(StringSlice file, ByteOffset offset) {
    if(offset.end > file.len) {
        fprintf(stderr, "byte offset is bigger than file length\n");
        exit(EXIT_FAILURE);
    }

    u64 line = 1;
    u64 column = 1;
    for(u64 i = 0; i < offset.start; i++) {
        if(file.ptr[i] == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
    }

    Location loc = { .line = line, .column = column };
    return loc;
}

char* fmt_location(
    Allocator* allocator,
    Location loc,
    u64 indentation
) {
    StringBuilder builder = string_builder_create(allocator);
    string_builder_write_string(&builder, "Location{\n");
    indentation++;

    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".line = %lu,\n",
        loc.line
    );

    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".column = %lu,\n",
        loc.column
    );

    indentation--;
    string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, "}");

    return string_builder_to_string(&builder);
}

char* fmt_byte_offset(
    Allocator* allocator,
    ByteOffset byte_offset,
    u64 indentation
) {
    StringBuilder builder = string_builder_create(allocator);
    string_builder_write_string(&builder, "ByteOffset{\n");
    indentation++;

    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".start = %lu,\n",
        byte_offset.start
    );

    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".end = %lu,\n",
        byte_offset.end
    );

    indentation--;
    string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, "}");

    return string_builder_to_string(&builder);
}
