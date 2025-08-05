#include <stdio.h>

#include "lexer.h"
#include "parser.h"
#include "string/string_slice.h"
#include "mem/arena.h"
#include "mem/allocator.h"
#include "typer.h"

StringSlice read_file(Arena* arena, const char* path) {
    FILE* file = fopen(path, "r");

    if(!file) {
        perror("failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    u64 length = (u64)ftell(file);
    rewind(file);

    // Allocate memory for content + null terminator
    char* buffer = arena_alloc_many(arena, char, length + 1);
    if(!buffer) {
        perror("failed to read file");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    u64 read = fread(buffer, 1, length, file);
    if(read != length) {
        fprintf(stderr, "failed to read entire file\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    buffer[read] = '\0'; // Null terminate the buffer
    fclose(file);

    return string_slice_create(buffer);
}

int main(void) {
    Arena* file_arena = (Arena*)(arena_create().ctx);
    StringSlice file = read_file(file_arena, "samples/operators.loy");

    Allocator lexer_arena = arena_create();
    TokenStream stream = lexer_tokenize_file(&lexer_arena, file);

    Allocator parser_arena = arena_create();
    Ast ast = parser_parse_token_stream(&parser_arena, &stream, file);
    // (void)ast;

    // vector_inspect(&stream.tokens, lexer_fmt_token);

    // vector_inspect(&ast.statements, parser_fmt_node);

    // typer_typecheck_ast(ast);

    // lexer_destroy(&stream);
    // parser_destroy(&ast);
    // free(file.ptr);

    arena_destroy(file_arena);
    arena_destroy((Arena*)lexer_arena.ctx);
    arena_destroy((Arena*)parser_arena.ctx);

    return 0;
}
