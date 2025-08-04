#include <stdio.h>

#include "defines.h"

void* malloc_bail(u64 node_size) {
    void* node = malloc(node_size);
    if(node == NULL) {
        fprintf(stderr, "Failed to allocate memory for AstNode\n");
        fprintf(stderr, "@ %s:%d", __FILE__, __LINE__);
        exit(EXIT_FAILURE);
    }
    return node;
}
