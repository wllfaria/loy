#ifndef _LEXER_H
#define _LEXER_H

#include "mem/allocator.h"
#include "string/string_slice.h"
#include "collections/vector.h"
#include "error.h"

typedef enum {
    TOKEN_EQUAL,
    TOKEN_COLON,
    TOKEN_ASSIGN_ADD,
    TOKEN_IDENT,
    TOKEN_LET,
    TOKEN_COMMA,
    TOKEN_FUN,

    TOKEN_RETURN,

    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_SEMI,

    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,

    TOKEN_THIN_ARROW,

    TOKEN_PLUS,
    TOKEN_PLUS_PLUS,
} TokenKind;

typedef struct {
    TokenKind  kind;
    char*      lexeme;
    bool       is_signed;
    ByteOffset byte_offset;
} Token;

typedef struct {
    Vector tokens;
    u64    pos;
} TokenStream;

LoyResult lexer_tokenize_file(
    TokenStream* stream,
    Allocator* allocator,
    CompileContext* ctx,
    StringSlice file
);

const char* lexer_fmt_token_kind(TokenKind kind);

char* lexer_fmt_token(Allocator* allocator, void* item, u64 indentation);
void lexer_destroy(TokenStream* stream);

#endif
