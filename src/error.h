#ifndef _ERROR_H
#define _ERROR_H

#include "defines.h"
#include "string/string_slice.h"
#include "mem/allocator.h"

typedef struct {
    u64 line;
    u64 column;
} Location;

typedef struct {
    u64 start;
    u64 end;
} ByteOffset;

Location location_from_byte_offset(StringSlice file, ByteOffset offset);

char* fmt_location(Allocator* allocator, Location loc, u64 indentation);
char* fmt_byte_offset(
    Allocator* allocator,
    ByteOffset byte_offset,
    u64 indentation
);

#endif
