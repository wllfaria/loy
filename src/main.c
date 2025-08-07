#include <stdio.h>

#include "defines.h"
#include "error.h"
#include "lexer.h"
#include "parser.h"
#include "string/string_slice.h"
#include "mem/arena.h"
#include "mem/allocator.h"
#include "typer.h"

static CompileContext compile_context_create(
    Allocator* allocator,
    CompilationUnit unit
) {
    CompileContext ctx = {
        .report = { .spans = vector_create(allocator) },
        .unit = unit,
    };

    return ctx;
}

LoyResult read_file(StringSlice* out, Arena* arena, const char* path) {
    FILE* file = fopen(path, "r");
    if(file == NULL) return LOY_ERROR_IO;

    fseek(file, 0, SEEK_END);
    u64 length = (u64)ftell(file);
    rewind(file);

    char* buffer = arena_alloc_many(arena, char, length + 1);
    if(buffer == NULL) return LOY_ERROR_ALLOC;

    u64 read = fread(buffer, 1, length, file);
    if(read != length) return LOY_ERROR_IO;

    buffer[read] = '\0'; // Null terminate the buffer
    fclose(file);

    *out = string_slice_create(buffer);
    return LOY_OK;
}

int main(void) {
    Allocator compile_alloc, lexer_alloc, parser_alloc, typer_alloc;
    LoyResult file_alloc_result = arena_create(&compile_alloc);
    LoyResult lexer_alloc_result = arena_create(&lexer_alloc);
    LoyResult parser_alloc_result = arena_create(&parser_alloc);
    LoyResult typer_alloc_result = arena_create(&typer_alloc);

    if(
        file_alloc_result != LOY_OK ||
        lexer_alloc_result != LOY_OK ||
        parser_alloc_result != LOY_OK ||
        typer_alloc_result != LOY_OK
    ) {
        fprintf(stderr, "Failed to setup arenas");
        goto cleanup;
    }

    const char* path = "samples/operators.loy";
    StringSlice file;
    if(read_file(&file, compile_alloc.ctx, path) != LOY_OK) {
        fprintf(stderr, "Failed to read file");
        goto cleanup;
    }

    CompilationUnit unit = {
        .content = file,
        .path = path,
        .relative_path = path,
    };

    CompileContext ctx = compile_context_create(&compile_alloc, unit);

    TokenStream stream = { 0 };
    if(lexer_tokenize_file(&stream, &lexer_alloc, &ctx, file) != LOY_OK) goto cleanup;

    Ast ast = { .statements = vector_create(&parser_alloc) };
    LoyResult parser_result = parser_parse_token_stream(&ast, &parser_alloc, &ctx, &stream);
    if(parser_result != LOY_OK) goto cleanup;

    TypedAst typed_ast = { .statements = vector_create(&typer_alloc) };
    if(typer_typecheck_ast(&ctx, &typed_ast, &typer_alloc, ast) != LOY_OK) goto cleanup;

cleanup:
    error_report_print(&ctx.report);
    arena_destroy(compile_alloc.ctx);
    arena_destroy(lexer_alloc.ctx);
    arena_destroy(parser_alloc.ctx);
    arena_destroy(typer_alloc.ctx);

    return 0;
}
