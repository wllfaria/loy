#include <stdio.h>

#include "defines.h"
#include "error.h"
#include "mem/arena.h"
#include "string/string_builder.h"
#include "string/string_slice.h"

ByteOffset byte_offset_merge(ByteOffset a, ByteOffset b) {
    ByteOffset new = { .start = a.start, .end = b.end };
    return new;
}

Location location_from_byte_offset(StringSlice file, ByteOffset offset) {
    if(offset.end > file.len) {
        fprintf(stderr, "byte offset is bigger than file length\n");
        exit(EXIT_FAILURE);
    }

    u64 line_start = 1;
    u64 column_start = 1;
    u64 line_end = 1;
    u64 column_end = 1;

    for(u64 i = 0; i < offset.start; i++) {
        if(file.ptr[i] == '\n') {
            line_start++;
            column_start = 1;
        } else {
            column_start++;
        }
    }

    for(u64 i = 0; i < offset.end; i++) {
        if(file.ptr[i] == '\n') {
            line_end++;
            column_end = 1;
        } else {
            column_end++;
        }
    }

    Location loc = {
        .line_start = line_start,
        .column_start = column_start,
        .line_end = line_end,
        .column_end = column_end
    };

    return loc;
}

LoyResult error_report_push_span(Report* report, Span span) {
    vector_push(&report->spans, span);
    return LOY_OK;
}

static char* format_span_level(Allocator* allocator, SpanLevel level) {
    StringBuilder builder = string_builder_create(allocator);

    switch(level) {
    case SPAN_WARN: {
        string_builder_write_string(&builder, "WARNING");
        break;
    }
    case SPAN_ERROR: {
        string_builder_write_string(&builder, "ERROR");
        break;
    }
    }

    return string_builder_to_string(&builder);
}

LoyResult error_report_print(Report* report) {
    Allocator allocator;
    if(arena_create(&allocator) != LOY_OK) return LOY_ERROR_ALLOC;

    StringBuilder builder = string_builder_create(&allocator);

    VectorIter span_iter = vector_iter(&report->spans);
    while(vector_iter_peek(&span_iter) != NULL) {
        Span* span = (Span*)vector_iter_next(&span_iter);

        Location location = location_from_byte_offset(
            span->file,
            span->offset
        );

        string_builder_write_format(
            &builder,
            "[%s]: ",
            format_span_level(&allocator, span->level)
        );

        string_builder_write_format(
            &builder,
            "%s\n",
            span->label
        );

        u64 line_num_width = 2;
        u64 line_start = location.line_start;
        while(line_start > 9) {
            line_start /= 10;
            line_num_width++;
        }

        string_builder_write_format(
            &builder,
            "@ foo.loy:%lu:%lu:\n",
            location.line_start,
            location.column_start
        );

        for(u64 i = 0; i < line_num_width; i++) {
            string_builder_write_byte(&builder, ' ');
        }
        string_builder_write_string(&builder, "|\n");

        string_builder_write_format(&builder, "%lu ", location.line_start);

        string_builder_write_format(
            &builder,
            "| "
        );

        StringSlice line = string_slice_get_line(
            span->file,
            location.line_start - 1
        );

        string_builder_write_format(
            &builder,
            "%.*s\n",
            line.len,
            line.ptr
        );

        for(u64 i = 0; i < line_num_width; i++) {
            string_builder_write_byte(&builder, ' ');
        }

        u64 underline_len = location.column_end - location.column_start;
        if(underline_len == 0) underline_len = 1; // at least one ^ if it's a point

        string_builder_write_string(&builder, "| ");
        for(u64 i = 1; i < location.column_start; i++) {
            string_builder_write_byte(&builder, ' ');
        }

        for(u64 i = 0; i < underline_len; i++) {
            string_builder_write_byte(&builder, '^');
        }

        if(span->info != NULL) {
            string_builder_write_format(&builder, " %s", span->info);
        }
    }

    fprintf(stderr, "%s\n", string_builder_to_string(&builder));

    arena_destroy((Arena*)allocator.ctx);
    return LOY_OK;
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
        ".line_start = %lu,\n",
        loc.line_start
    );
    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".line_end = %lu,\n",
        loc.line_end
    );

    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".column_start = %lu,\n",
        loc.column_start
    );

    string_builder_indent(&builder, indentation);
    string_builder_write_format(
        &builder,
        ".column_end = %lu,\n",
        loc.column_end
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
