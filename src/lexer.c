#include <stdio.h>

#include "defines.h"
#include "lexer.h"
#include "string/string_builder.h"
#include "unicode/ascii.h"

typedef struct {
    const char* keyword;
    TokenKind   kind;
} Keyword;

static Keyword keywords[] = {
    { "let", TOKEN_LET },
    { "fun", TOKEN_FUN },
};

static TokenKind lookup_keyword(const char* ident) {
    for(u64 i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
        if(strcmp(ident, keywords[i].keyword) == 0) {
            return keywords[i].kind;
        }
    }
    return TOKEN_IDENT;
}

static bool is_valid_identifier_starter(char ch) {
    return is_ascii_alphabetic(ch) || ch == '_';
}

static bool is_valid_identifier_character(char ch) {
    return is_ascii_alphanumeric(ch) || ch == '_';
}

static const char* lexer_fmt_token_kind(TokenKind kind) {
    switch(kind) {
    case TOKEN_ASSIGN_ADD: return "TOKEN_ASSIGN_ADD";
    case TOKEN_EQUAL: return "TOKEN_ASSIGN";
    case TOKEN_IDENT: return "TOKEN_IDENT";
    case TOKEN_LET: return "TOKEN_LET";
    case TOKEN_PLUS: return "TOKEN_PLUS";
    case TOKEN_PLUS_PLUS: return "TOKEN_PLUS_PLUS";
    case TOKEN_COLON: return "TOKEN_COLON";
    case TOKEN_INT: return "TOKEN_INT";
    case TOKEN_FLOAT: return "TOKEN_FLOAT";
    case TOKEN_SEMI: return "TOKEN_SEMI";
    case TOKEN_FUN: return "TOKEN_FUN";
    case TOKEN_LPAREN: return "TOKEN_LPAREN";
    case TOKEN_RPAREN: return "TOKEN_RPAREN";
    case TOKEN_LBRACE: return "TOKEN_LPAREN";
    case TOKEN_RBRACE: return "TOKEN_RPAREN";
    case TOKEN_THIN_ARROW: return "TOKEN_THIN_ARROW";
    case TOKEN_COMMA: return "TOKEN_COMMA";
    }
}

char* lexer_fmt_token(void* item, u64 indentation) {
    Token* token = (Token*)item;
    StringBuilder builder = string_builder_create();
    const char* kind = lexer_fmt_token_kind(token->kind);

    string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, "Token{\n");
    string_builder_indent(&builder, indentation + 1);

    string_builder_write_format(&builder, ".kind = %s,\n", kind);
    string_builder_indent(&builder, indentation + 1);
    string_builder_write_format(&builder, ".lexeme = \"%s\",\n", token->lexeme);

    string_builder_indent(&builder, indentation);
    string_builder_write_byte(&builder, '}');

    return string_builder_to_string(&builder);
}

static Token tokenize_identifier(StringChars* chars, char start_ch) {
    StringBuilder builder = string_builder_create();

    string_builder_write_byte(&builder, start_ch);

    for(;;) {
        char peeked = string_chars_peek(chars);
        if(!is_valid_identifier_character(peeked)) break;
        string_chars_next(chars);
        string_builder_write_byte(&builder, peeked);
    }

    char* lexeme = string_builder_to_string(&builder);
    TokenKind kind = lookup_keyword(lexeme);

    Token tok = { .kind = kind, .lexeme = lexeme };
    return tok;
}

static void tokenize_digits(StringChars* chars, StringBuilder* builder) {
    char peeked = string_chars_peek(chars);

    while(is_digit(peeked) || peeked == '_') {
        string_builder_write_byte(builder, string_chars_next(chars));
        peeked = string_chars_peek(chars);
    }
}

static Token tokenize_number(StringChars* chars, char start_ch) {
    StringBuilder builder = string_builder_create();
    TokenKind kind = TOKEN_INT;
    bool is_signed = false;

    if(start_ch == '-') is_signed = true;
    string_builder_write_byte(&builder, start_ch);
    tokenize_digits(chars, &builder);

    // If the number has a dot <.>, we try to parse further digits as it is a
    // float
    if(string_chars_peek(chars) == '.') {
        kind = TOKEN_FLOAT;
        string_builder_write_byte(&builder, string_chars_next(chars));
        tokenize_digits(chars, &builder);
    }

    Token tok = {
        .kind      = kind,
        .lexeme    = string_builder_to_string(&builder),
        .is_signed = is_signed,
    };

    return tok;
}

static Token make_token(TokenKind kind, char* lexeme) {
    Token tok = { .kind = kind, .lexeme = lexeme };
    return tok;
}

static void consume_until_newline(StringChars* chars) {
    for(;;) {
        char peeked = string_chars_peek(chars);
        if(peeked == EOF || peeked == '\n') break;
        string_chars_next(chars);
    }
}

TokenStream lexer_tokenize_file(StringSlice file) {
    StringChars chars = string_slice_chars(file);
    Vector tokens = vector_create();

    for(;;) {
        char curr = string_chars_next(&chars);
        char next = string_chars_peek(&chars);

        if(curr == EOF) break;
        if(is_ascii_whitespace(curr)) continue;

        // Double character tokens
        if(curr == '+' && next == '=') {
            string_chars_next(&chars); // Consume peeked token
            vector_push(&tokens, make_token(TOKEN_ASSIGN_ADD, "+="));
            continue;
        }

        if(curr == '+' && next == '+') {
            string_chars_next(&chars); // Consume peeked token
            vector_push(&tokens, make_token(TOKEN_PLUS_PLUS, "++"));
            continue;
        }

        if(curr == '-' && next == '>') {
            string_chars_next(&chars); // Consume peeked token
            vector_push(&tokens, make_token(TOKEN_THIN_ARROW, "->"));
            continue;
        }

        if(curr == '/' && next == '/') {
            consume_until_newline(&chars);
            continue;
        }

        if(curr == '-' && is_digit(next)) {
            vector_push(&tokens, tokenize_number(&chars, curr));
            continue;
        }

        // Single character tokens
        if(curr == '+') {
            vector_push(&tokens, make_token(TOKEN_PLUS, "+"));
            continue;
        }

        if(curr == ',') {
            vector_push(&tokens, make_token(TOKEN_COMMA, ","));
            continue;
        }

        if(curr == '=') {
            vector_push(&tokens, make_token(TOKEN_EQUAL, "="));
            continue;
        }

        if(curr == ':') {
            vector_push(&tokens, make_token(TOKEN_COLON, ":"));
            continue;
        }

        if(curr == ';') {
            vector_push(&tokens, make_token(TOKEN_SEMI, ";"));
            continue;
        }

        if(curr == '(') {
            vector_push(&tokens, make_token(TOKEN_LPAREN, "("));
            continue;
        }

        if(curr == ')') {
            vector_push(&tokens, make_token(TOKEN_RPAREN, ")"));
            continue;
        }

        if(curr == '{') {
            vector_push(&tokens, make_token(TOKEN_LBRACE, "{"));
            continue;
        }

        if(curr == '}') {
            vector_push(&tokens, make_token(TOKEN_RBRACE, "}"));
            continue;
        }

        if(is_valid_identifier_starter(curr)) {
            vector_push(&tokens, tokenize_identifier(&chars, curr));
            continue;
        }

        if(is_digit(curr)) {
            vector_push(&tokens, tokenize_number(&chars, curr));
            continue;
        }

        printf("Unknown token %c\n", curr);
        exit(EXIT_SUCCESS);
    }

    TokenStream stream = { .tokens = tokens };
    return stream;
}

static void lexer_destroy_token(void* item) {
    Token* token = (Token*)item;

    switch(token->kind) {
    case TOKEN_EQUAL:
    case TOKEN_COLON:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
    case TOKEN_LPAREN:
    case TOKEN_RPAREN:
    case TOKEN_ASSIGN_ADD:
    case TOKEN_SEMI:
    case TOKEN_THIN_ARROW:
    case TOKEN_PLUS:
    case TOKEN_PLUS_PLUS:
    case TOKEN_COMMA:
        break;
    case TOKEN_IDENT:
    case TOKEN_LET:
    case TOKEN_FUN:
    case TOKEN_INT:
    case TOKEN_FLOAT:
        free((char*)token->lexeme);
        break;
    }
}

void lexer_destroy(TokenStream* stream) {
    vector_destroy(&stream->tokens, lexer_destroy_token);
}
