#include <stdio.h>

#include "lexer.h"
#include "parser.h"
#include "string/string_slice.h"
#include "typer.h"

StringSlice read_file(const char* path) {
    FILE* file = fopen(path, "r");

    if(!file) {
        perror("failed to open file");
        exit(EXIT_SUCCESS);
    }

    fseek(file, 0, SEEK_END);
    u64 length = (u64)ftell(file);
    rewind(file);

    // Allocate memory for content + null terminator
    char* buffer = malloc_bail(length + 1);
    if(!buffer) {
        perror("failed to read file");
        fclose(file);
        exit(EXIT_SUCCESS);
    }

    u64 read = fread(buffer, 1, length, file);
    buffer[read] = '\0'; // Null terminate the buffer
    fclose(file);

    return string_slice_create(buffer);
}

int main(void) {
    StringSlice file = read_file("samples/operators.loy");
    TokenStream stream = lexer_tokenize_file(file);
    Ast ast = parser_parse_token_stream(&stream);

    // vector_inspect(&ast.statements, parser_fmt_node);

    typer_typecheck_ast(ast);

    // lexer_destroy(&stream);
    // parser_destroy(&ast);
    // free(file.ptr);
    return 0;
}
