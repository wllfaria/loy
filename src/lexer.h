#ifndef _LEXER_H
#define _LEXER_H

#include "string/string_slice.h"
#include "collections/vector.h"

typedef enum {
    TOKEN_EQUAL,
    TOKEN_COLON,
    TOKEN_ASSIGN_ADD,
    TOKEN_IDENT,
    TOKEN_LET,
    TOKEN_COMMA,
    TOKEN_FUN,

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
    TokenKind kind;
    char*     lexeme;
    bool      is_signed;
} Token;

typedef struct {
    Vector tokens;
    u64    pos;
} TokenStream;

TokenStream lexer_tokenize_file(StringSlice file);
char* lexer_fmt_token(void* token, u64 indentation);
void lexer_destroy(TokenStream* stream);

#endif
