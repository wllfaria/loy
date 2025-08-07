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
    { "return", TOKEN_RETURN },
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

const char* lexer_fmt_token_kind(TokenKind kind) {
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
    case TOKEN_RETURN: return "TOKEN_RETURN";
    }
    return "";
}

char* lexer_fmt_token(Allocator* allocator, void* item, u64 indentation) {
    Token* token = (Token*)item;
    StringBuilder builder = string_builder_create(allocator);
    const char* kind = lexer_fmt_token_kind(token->kind);

    string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, "Token{\n");
    indentation++;
    string_builder_indent(&builder, indentation);

    string_builder_write_format(&builder, ".kind = %s,\n", kind);
    string_builder_indent(&builder, indentation);
    string_builder_write_format(&builder, ".lexeme = \"%s\",\n", token->lexeme);

    string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, ".byte_offset = ");
    char* byte_offset = fmt_byte_offset(allocator, token->byte_offset, indentation);
    string_builder_write_string(&builder, byte_offset);
    string_builder_write_string(&builder, ",\n");

    indentation--;
    string_builder_indent(&builder, indentation);
    string_builder_write_byte(&builder, '}');

    return string_builder_to_string(&builder);
}

static Token make_token(TokenKind kind, char* lexeme, u64 start, u64 len) {
    u64 offset_end = start + len;
    Token tok = {
        .kind = kind,
        .lexeme = lexeme,
        .byte_offset = { .start = start, .end = offset_end },
    };
    return tok;
}

static Token tokenize_identifier(
    Allocator* allocator,
    StringChars* chars,
    char start_ch
) {
    StringBuilder builder = string_builder_create(allocator);
    u64 start = chars->cursor - 1;

    string_builder_write_byte(&builder, start_ch);

    for(;;) {
        char peeked = string_chars_peek(chars);
        if(!is_valid_identifier_character(peeked)) break;
        string_chars_next(chars);
        string_builder_write_byte(&builder, peeked);
    }

    char* lexeme = string_builder_to_string(&builder);
    TokenKind kind = lookup_keyword(lexeme);
    u64 len = chars->cursor - start;
    return make_token(kind, lexeme, start, len);
}

static void tokenize_digits(StringChars* chars, StringBuilder* builder) {
    char peeked = string_chars_peek(chars);

    while(is_digit(peeked) || peeked == '_') {
        string_builder_write_byte(builder, string_chars_next(chars));
        peeked = string_chars_peek(chars);
    }
}

static Token tokenize_number(
    Allocator* allocator,
    StringChars* chars,
    char start_ch
) {
    StringBuilder builder = string_builder_create(allocator);
    TokenKind kind = TOKEN_INT;
    bool is_signed = false;
    u64 start = chars->cursor - 1;

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

    char* lexeme = string_builder_to_string(&builder);
    u64 len = chars->cursor - start;
    Token tok = make_token(kind, lexeme, start, len);
    tok.is_signed = is_signed;
    return tok;
}

static void consume_until_newline(StringChars* chars) {
    for(;;) {
        char peeked = string_chars_peek(chars);
        if(peeked == EOF || peeked == '\n') break;
        string_chars_next(chars);
    }
}

LoyResult lexer_tokenize_file(
    TokenStream* stream,
    Allocator* allocator,
    CompileContext* ctx,
    StringSlice file
) {
    StringChars chars = string_slice_chars(file);
    Vector tokens = vector_create(allocator);

    for(;;) {
        u64 start = chars.cursor;
        char curr = string_chars_next(&chars);
        char next = string_chars_peek(&chars);

        if(curr == EOF) break;
        if(is_ascii_whitespace(curr)) continue;

        // Double character tokens
        if(curr == '+' && next == '=') {
            string_chars_next(&chars); // Consume peeked token
            Token token = make_token(TOKEN_ASSIGN_ADD, "+=", start, 2);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '+' && next == '+') {
            string_chars_next(&chars); // Consume peeked token
            Token token = make_token(TOKEN_PLUS_PLUS, "++", start, 2);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '-' && next == '>') {
            string_chars_next(&chars); // Consume peeked token
            Token token = make_token(TOKEN_THIN_ARROW, "->", start, 2);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '/' && next == '/') {
            consume_until_newline(&chars);
            continue;
        }

        if(curr == '-' && is_digit(next)) {
            vector_push(&tokens, tokenize_number(allocator, &chars, curr));
            continue;
        }

        // Single character tokens
        if(curr == '+') {
            Token token = make_token(TOKEN_PLUS, "+", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == ',') {
            Token token = make_token(TOKEN_COMMA, ",", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '=') {
            Token token = make_token(TOKEN_EQUAL, "=", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == ':') {
            Token token = make_token(TOKEN_COLON, ":", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == ';') {
            Token token = make_token(TOKEN_SEMI, ";", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '(') {
            Token token = make_token(TOKEN_LPAREN, "(", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == ')') {
            Token token = make_token(TOKEN_RPAREN, ")", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '{') {
            Token token = make_token(TOKEN_LBRACE, "{", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(curr == '}') {
            Token token = make_token(TOKEN_RBRACE, "}", start, 1);
            vector_push(&tokens, token);
            continue;
        }

        if(is_valid_identifier_starter(curr)) {
            vector_push(&tokens, tokenize_identifier(allocator, &chars, curr));
            continue;
        }

        if(is_digit(curr)) {
            vector_push(&tokens, tokenize_number(allocator, &chars, curr));
            continue;
        }

        ByteOffset offset = {
            .start = chars.cursor - 1,
            .end = chars.cursor,
        };

        Span span = {
            .file = file,
            .offset = offset,
            .label = "Unknown token",
            .level = SPAN_ERROR,
            .info = "Unexpected character",
        };

        if(error_report_push_span(&ctx->report, span) != LOY_OK) {
            return LOY_ERROR_ALLOC;
        }

        return LOY_ERROR_LEXER_INVALID_TOKEN;
    }

    stream->tokens = tokens;
    return LOY_OK;
}
