#include <stdio.h>

#include "parser.h"
#include "mem/generic.h"
#include "string/string_builder.h"
#include "mem/arena.h"

#define BASE_PRECEDENCE     0
#define PRECEDENCE_POSTFIX  2
#define PRECEDENCE_PREFIX   3
#define PRECEDENCE_ADDITION 6

static u8 token_operator_precedence(Token* operator, bool is_postfix) {
    switch(operator->kind) {
    case TOKEN_PLUS: {
        return PRECEDENCE_ADDITION;
    }
    case TOKEN_PLUS_PLUS: {
        if(is_postfix) return PRECEDENCE_POSTFIX;
        else return PRECEDENCE_PREFIX;
    }
    case TOKEN_LPAREN: {
        return PRECEDENCE_POSTFIX;
    }
    default: break;
    }

    UNREACHABLE();
}

Token* token_stream_peek_non_eof(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter,
    const char* expected
) {
    Token* peeked = vector_iter_peek(iter);

    if(peeked == NULL) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "expected `%s` but found `EOF`", expected);

        Span span = {
            .file = ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Unexpected token",
            .info = string_builder_to_string(&builder),
        };

        error_report_push_span(&ctx->report, span);
    }

    return peeked;
}

static Token* assert_token_kind(
    Allocator* allocator,
    VectorIter* iter,
    TokenKind expected,
    CompileContext* ctx,
    char* fmt
) {
    Token* needle = (Token*)vector_iter_next(iter);

    if(needle == NULL) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "expected `%s` but found `EOF`", fmt);

        Span span = {
            .file = ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Invalid token",
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&ctx->report, span);
        return NULL;
    }

    if(needle->kind != expected) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "expected `%s` but found `%s`", fmt, needle->lexeme);

        Span span = {
            .file = ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Invalid token",
            .info = string_builder_to_string(&builder),
            .offset = needle->byte_offset,
        };

        error_report_push_span(&ctx->report, span);

        return NULL;
    }

    return needle;
}

static AstIdentNode* parser_parse_identifier(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    Token* identifier = assert_token_kind(allocator, iter, TOKEN_IDENT, ctx, "identifier");
    if(identifier == NULL) return NULL;

    AstIdentNode* node = allocator->alloc(allocator->ctx, sizeof(AstIdentNode));
    if(node == NULL) return NULL;

    node->tag.kind = AST_NODE_IDENTIFIER;
    node->tag.byte_offset = identifier->byte_offset;
    node->name = identifier->lexeme;
    return node;
}

static AstTypeNode* parser_parse_type_annotation(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    Token* colon = assert_token_kind(allocator, iter, TOKEN_COLON, ctx, ":");
    if(colon == NULL) return NULL;

    AstIdentNode* ident = parser_parse_identifier(allocator, ctx, iter);
    if(ident == NULL) return NULL;

    AstTypeNode* annotation = allocator->alloc(allocator->ctx, sizeof(AstTypeNode));
    if(annotation == NULL) return NULL;

    annotation->tag.kind = AST_NODE_TYPE_ANNOTATION;
    annotation->tag.byte_offset = byte_offset_merge(colon->byte_offset, ident->tag.byte_offset);
    annotation->ident = ident;
    return annotation;
}

static AstFunArgNode* parser_parse_function_arg(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    AstIdentNode* name = parser_parse_identifier(allocator, ctx, iter);
    if(name == NULL) return NULL;

    AstTypeNode* type = parser_parse_type_annotation(allocator, ctx, iter);
    if(type == NULL) return NULL;

    AstFunArgNode* node = allocator->alloc(allocator->ctx, sizeof(AstFunArgNode));
    if(node == NULL) return NULL;

    node->tag.kind = AST_NODE_FUN_ARG;
    node->tag.byte_offset = byte_offset_merge(name->tag.byte_offset, type->tag.byte_offset);
    node->type = type;
    node->ident = name;

    return node;
}

// TODO(wiru): remove underscores from numbers
static AstNode* parser_parse_number_literal(
    Allocator* allocator,
    VectorIter* iter
) {
    Token* token = (Token*)vector_iter_next(iter);

    if(token->kind == TOKEN_INT) {
        if(token->is_signed) {
            i64 value = strtoll(token->lexeme, NULL, 10);
            AstIntNode* expr = allocator->alloc(allocator->ctx, sizeof(AstIntNode));
            if(expr == NULL) return NULL;

            expr->tag.kind = AST_NODE_INT_LITERAL;
            expr->tag.byte_offset = token->byte_offset;
            expr->value = value;
            return (AstNode*)expr;
        }

        u64 value = strtoull(token->lexeme, NULL, 10);
        AstUintNode* expr = allocator->alloc(allocator->ctx, sizeof(AstUintNode));
        expr->tag.kind = AST_NODE_UINT_LITERAL;
        expr->tag.byte_offset = token->byte_offset;
        expr->value = value;
        return (AstNode*)expr;
    }

    if(token->kind == TOKEN_FLOAT) {
        AstFloatNode* expr = allocator->alloc(allocator->ctx, sizeof(AstFloatNode));
        if(expr == NULL) return NULL;
        expr->tag.kind = AST_NODE_FLOAT_LITERAL;
        expr->tag.byte_offset = token->byte_offset;
        expr->value = token->lexeme;
        return (AstNode*)expr;
    }

    UNREACHABLE();
}

static bool token_is_operator(Token* token) {
    switch(token->kind) {
    case TOKEN_PLUS:
    case TOKEN_LPAREN:
    case TOKEN_PLUS_PLUS:
        return true;

    case TOKEN_EQUAL:
    case TOKEN_COLON:
    case TOKEN_ASSIGN_ADD:
    case TOKEN_IDENT:
    case TOKEN_LET:
    case TOKEN_FUN:
    case TOKEN_INT:
    case TOKEN_FLOAT:
    case TOKEN_SEMI:
    case TOKEN_RPAREN:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
    case TOKEN_THIN_ARROW:
    case TOKEN_COMMA:
    case TOKEN_RETURN:
        return false;
    }
    return false;
}

static AstNode* parser_parse_with_precedence(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter,
    u8 precedence
);

static LoyResult parser_parse_function_call(
    Statements* out,
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    Token* peeked = token_stream_peek_non_eof(allocator, ctx, iter, "idk");
    u64 pos = 0;
    while(peeked != NULL && peeked->kind != TOKEN_RPAREN) {
        if(pos != 0 && assert_token_kind(allocator, iter, TOKEN_COMMA, ctx, ",") == NULL) {
            return LOY_ERROR_PARSER_INVALID_TOKEN;
        }

        Token* next = vector_iter_peek(iter);
        if(next != NULL && next->kind == TOKEN_RPAREN) break;

        AstNode* arg = parser_parse_with_precedence(allocator, ctx, iter, BASE_PRECEDENCE);
        if(arg == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

        vector_push_ptr(out, arg);

        peeked = vector_iter_peek(iter);
        pos++;
    }

    return LOY_OK;
}

static AstNode* parser_parse_with_precedence(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter,
    u8 precedence
) {
    Token* token = (Token*)vector_iter_peek(iter);
    AstNode* lhs = NULL;

    switch(token->kind) {
    case TOKEN_INT: {
        lhs = parser_parse_number_literal(allocator, iter);
        if(lhs == NULL) return NULL;
        break;
    }
    case TOKEN_FLOAT: {
        lhs = parser_parse_number_literal(allocator, iter);
        if(lhs == NULL) return NULL;
        break;
    }
    case TOKEN_IDENT: {
        lhs = (AstNode*)parser_parse_identifier(allocator, ctx, iter);
        if(lhs == NULL) return NULL;
        break;
    }
    case TOKEN_LPAREN:
    case TOKEN_PLUS:
    case TOKEN_PLUS_PLUS:
        break;

    case TOKEN_EQUAL:
    case TOKEN_RETURN:
    case TOKEN_COLON:
    case TOKEN_ASSIGN_ADD:
    case TOKEN_LET:
    case TOKEN_FUN:
    case TOKEN_SEMI:
    case TOKEN_RPAREN:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
    case TOKEN_THIN_ARROW:
    case TOKEN_COMMA: {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "Token `%s` (%s) is not valid here",
            token->lexeme,
            lexer_fmt_token_kind(token->kind)
        );

        Span span = {
            .file = ctx->unit.content,
            .label = "Invalid token",
            .level = SPAN_ERROR,
            .offset = token->byte_offset,
            .info = string_builder_to_string(&builder),
        };

        error_report_push_span(&ctx->report, span);
        return NULL;
    }
    }

    for(;;) {
        Token* peeked = token_stream_peek_non_eof(allocator, ctx, iter, "an operator");
        if(!token_is_operator(peeked)) break;

        switch(peeked->kind) {
        case TOKEN_LPAREN: {
            if(assert_token_kind(allocator, iter, TOKEN_LPAREN, ctx, "(") == NULL) return NULL;

            Statements call_args = vector_create(allocator);
            if(parser_parse_function_call(&call_args, allocator, ctx, iter) != LOY_OK) return NULL;

            Token* closing_paren = assert_token_kind(allocator, iter, TOKEN_RPAREN, ctx, ")");
            if(closing_paren == NULL) return NULL;

            AstFunCallNode* call = allocator->alloc(allocator->ctx, sizeof(AstFunCallNode));
            if(call == NULL) return NULL;

            call->tag.kind = AST_NODE_FUN_CALL;
            call->tag.byte_offset = byte_offset_merge(lhs->byte_offset, closing_paren->byte_offset);
            call->args = call_args;
            call->ident = (AstIdentNode*)lhs;
            return (AstNode*)call;
        }
        default: break;
        }

        u8 op_precedence = token_operator_precedence(peeked, true);
        if(op_precedence <= precedence) break;
        Token* op = vector_iter_next(iter);

        AstNode* rhs = parser_parse_with_precedence(allocator, ctx, iter, op_precedence);
        if(rhs == NULL) return NULL;

        AstBinaryOpNode* binop = allocator->alloc(allocator->ctx, sizeof(AstBinaryOpNode));
        if(binop == NULL) return NULL;

        binop->tag.kind = AST_NODE_BINARY_OP;
        binop->lhs = lhs;
        binop->rhs = rhs;
        binop->op = op->kind;
        lhs = (AstNode*)binop;
    }

    return lhs;
}

static AstNode* parser_parse_expression(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    return parser_parse_with_precedence(allocator, ctx, iter, BASE_PRECEDENCE);
}

static AstNode* parser_parse_let_binding(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    if(assert_token_kind(allocator, iter, TOKEN_LET, ctx, "let") == NULL) return NULL;

    AstIdentNode* name = parser_parse_identifier(allocator, ctx, iter);
    if(name == NULL) return NULL;

    AstTypeNode* type = parser_parse_type_annotation(allocator, ctx, iter);
    if(type == NULL) return NULL;

    if(assert_token_kind(allocator, iter, TOKEN_EQUAL, ctx, "=") == NULL) return NULL;

    AstNode* value = parser_parse_expression(allocator, ctx, iter);
    if(value == NULL) return NULL;

    if(assert_token_kind(allocator, iter, TOKEN_SEMI, ctx, ";") == NULL) return NULL;

    AstLetNode* node = allocator->alloc(allocator->ctx, sizeof(AstLetNode));
    if(node == NULL) return NULL;

    node->ident = name;
    node->type = type;
    node->tag.kind = AST_NODE_LET_BINDING;
    node->value = value;
    return (AstNode*)node;
}

static AstReturnNode* parser_parse_return(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    Token* keyword = assert_token_kind(allocator, iter, TOKEN_RETURN, ctx, "return");
    if(keyword == NULL) return NULL;

    Token* peek = vector_iter_peek(iter);
    if(peek == NULL) return NULL;

    AstReturnNode* node = allocator->alloc(allocator->ctx, sizeof(AstReturnNode));
    if(node == NULL) return NULL;

    switch(peek->kind) {
    case TOKEN_LPAREN:
    case TOKEN_INT:
    case TOKEN_FLOAT:
    case TOKEN_IDENT: {
        node->return_value = parser_parse_with_precedence(allocator, ctx, iter, BASE_PRECEDENCE);
        break;
    }
    case TOKEN_SEMI: {
        node->return_value = NULL;
        break;
    }
    case TOKEN_EQUAL:
    case TOKEN_COLON:
    case TOKEN_ASSIGN_ADD:
    case TOKEN_LET:
    case TOKEN_COMMA:
    case TOKEN_FUN:
    case TOKEN_RETURN:
    case TOKEN_RPAREN:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
    case TOKEN_THIN_ARROW:
    case TOKEN_PLUS:
    case TOKEN_PLUS_PLUS:
        return NULL;
    }

    Token* semi = assert_token_kind(allocator, iter, TOKEN_SEMI, ctx, ";");
    if(semi == NULL) return NULL;

    node->tag.kind = AST_NODE_RETURN;
    node->tag.byte_offset = byte_offset_merge(keyword->byte_offset, semi->byte_offset);
    return node;
}

static LoyResult parser_parse_block(
    Statements* out,
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    Token* peeked = (Token*)vector_iter_peek(iter);

    while(peeked != NULL && peeked->kind != TOKEN_RBRACE) {
        switch(peeked->kind) {
        case TOKEN_LET: {
            AstNode* let_binding = parser_parse_let_binding(allocator, ctx, iter);
            if(let_binding == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;
            vector_push_ptr(out, let_binding);
            break;
        }
        case TOKEN_RETURN: {
            AstNode* return_stmt = (AstNode*)parser_parse_return(allocator, ctx, iter);
            if(return_stmt == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;
            vector_push_ptr(out, return_stmt);
            break;
        }
        case TOKEN_IDENT: {
            AstIdentNode* ident = parser_parse_identifier(allocator, ctx, iter);
            if(ident == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

            Token* next = vector_iter_peek(iter);
            if(next != NULL && next->kind == TOKEN_LPAREN) {
                if(assert_token_kind(allocator, iter, TOKEN_LPAREN, ctx, "(") == NULL) {
                    return LOY_ERROR_PARSER_INVALID_TOKEN;
                }

                Statements call_args = vector_create(allocator);
                LoyResult result = parser_parse_function_call(&call_args, allocator, ctx, iter);
                if(result != LOY_OK) return LOY_ERROR_PARSER_INVALID_TOKEN;

                Token* closing_paren = assert_token_kind(allocator, iter, TOKEN_RPAREN, ctx, ")");
                if(closing_paren == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

                if(assert_token_kind(allocator, iter, TOKEN_SEMI, ctx, ";") == NULL) {
                    return LOY_ERROR_PARSER_INVALID_TOKEN;
                }

                AstFunCallNode* call = allocator->alloc(allocator->ctx, sizeof(AstFunCallNode));
                if(call == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

                call->tag.kind = AST_NODE_FUN_CALL;
                call->tag.byte_offset = byte_offset_merge(ident->tag.byte_offset, closing_paren->byte_offset);
                call->args = call_args;
                call->ident = ident;
                vector_push_ptr(out, (AstNode*)call);
            }

            break;
        }
        case TOKEN_EQUAL:
        case TOKEN_COLON:
        case TOKEN_ASSIGN_ADD:
        case TOKEN_FUN:
        case TOKEN_INT:
        case TOKEN_FLOAT:
        case TOKEN_SEMI:
        case TOKEN_LPAREN:
        case TOKEN_RPAREN:
        case TOKEN_LBRACE:
        case TOKEN_RBRACE:
        case TOKEN_THIN_ARROW:
        case TOKEN_PLUS:
        case TOKEN_PLUS_PLUS:
        case TOKEN_COMMA:
            UNREACHABLE();
        }

        peeked = (Token*)vector_iter_peek(iter);
    }

    return LOY_OK;
}

static AstFunNode* parser_parse_function(
    Allocator* allocator,
    CompileContext* ctx,
    VectorIter* iter
) {
    Token* keyword = assert_token_kind(allocator, iter, TOKEN_FUN, ctx, "fun");
    if(keyword == NULL) return NULL;

    AstIdentNode* identifier = parser_parse_identifier(allocator, ctx, iter);
    if(identifier == NULL) return NULL;

    if(assert_token_kind(allocator, iter, TOKEN_LPAREN, ctx, "(") == NULL) return NULL;

    Statements args = vector_create(allocator);
    Token* peeked = vector_iter_peek(iter);
    u64 pos = 0;
    while(peeked != NULL && peeked->kind != TOKEN_RPAREN) {
        if(pos != 0 && assert_token_kind(allocator, iter, TOKEN_COMMA, ctx, ",") == NULL) return NULL;

        Token* next = vector_iter_peek(iter);
        if(next != NULL && next->kind == TOKEN_RPAREN) break;

        AstFunArgNode* arg = parser_parse_function_arg(allocator, ctx, iter);
        if(arg == NULL) return NULL;

        vector_push_ptr(&args, arg);
        peeked = vector_iter_peek(iter);
        pos++;
    }

    if(assert_token_kind(allocator, iter, TOKEN_RPAREN, ctx, ")") == NULL) return NULL;
    if(assert_token_kind(allocator, iter, TOKEN_THIN_ARROW, ctx, "->") == NULL) return NULL;

    AstIdentNode* return_type = parser_parse_identifier(allocator, ctx, iter);
    if(return_type == NULL) return NULL;

    if(assert_token_kind(allocator, iter, TOKEN_LBRACE, ctx, "{") == NULL) return NULL;

    Statements block = vector_create(allocator);
    if(parser_parse_block(&block, allocator, ctx, iter) != LOY_OK) return NULL;

    Token* block_end = assert_token_kind(allocator, iter, TOKEN_RBRACE, ctx, "}");
    if(block_end == NULL) return NULL;

    AstFunNode* function = allocator->alloc(allocator->ctx, sizeof(AstFunNode));
    if(function == NULL) return NULL;

    function->tag.kind = AST_NODE_FUN;
    function->tag.byte_offset = byte_offset_merge(keyword->byte_offset, block_end->byte_offset);
    function->ident = (AstIdentNode*)identifier;
    function->args = args;
    function->return_type = return_type;
    function->body = block;

    return function;
}

LoyResult parser_parse_token_stream(
    Ast* ast,
    Allocator* allocator,
    CompileContext* ctx,
    TokenStream* stream
) {
    VectorIter iter = vector_iter(&stream->tokens);
    while(vector_iter_peek(&iter) != NULL) {
        Token* token = (Token*)vector_iter_peek(&iter);

        switch(token->kind) {
        case TOKEN_FUN: {
            AstNode* function = (AstNode*)parser_parse_function(allocator, ctx, &iter);
            if(function == NULL) return LOY_ERROR_LEXER_INVALID_TOKEN;
            vector_push_ptr(&ast->statements, function);
            break;
        }

        case TOKEN_EQUAL:
        case TOKEN_COLON:
        case TOKEN_ASSIGN_ADD:
        case TOKEN_IDENT:
        case TOKEN_LET:
        case TOKEN_INT:
        case TOKEN_FLOAT:
        case TOKEN_SEMI:
        case TOKEN_LPAREN:
        case TOKEN_RPAREN:
        case TOKEN_LBRACE:
        case TOKEN_RBRACE:
        case TOKEN_THIN_ARROW:
        case TOKEN_PLUS:
        case TOKEN_PLUS_PLUS:
        case TOKEN_COMMA:
            break;
        case TOKEN_RETURN:
            break;
        }
    }

    return LOY_OK;
}

char* parser_fmt_node(Allocator* allocator, void* item, u64 indentation) {
    AstNode* node = (AstNode*)item;
    StringBuilder builder = string_builder_create(allocator);

    switch(node->kind) {
    case AST_NODE_IDENTIFIER: {
        AstIdentNode* identifier = (AstIdentNode*)node;
        string_builder_write_format(&builder, "%s", identifier->name);
        break;
    }
    case AST_NODE_FUN_ARG: {
        AstFunArgNode* arg = (AstFunArgNode*)node;
        char* name = parser_fmt_node(allocator, arg->ident, indentation);
        char* type = parser_fmt_node(allocator, arg->type, indentation);

        string_builder_write_format(&builder, "%s: %s", name, type);
        break;
    }
    case AST_NODE_FUN: {
        AstFunNode* function = (AstFunNode*)node;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "AstFunNode{\n");

        char* name = parser_fmt_node(allocator, function->ident, 0);
        char* return_type = parser_fmt_node(
            allocator,
            function->return_type,
            0
        );

        indentation++;
        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, ".name = \"%s\",\n", name);
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".return type = ");
        string_builder_write_format(&builder, "\"%s\"\n", return_type);
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".args = [");
        if(function->args.len > 0) string_builder_write_string(&builder, "\n");

        u8 pos = 0;
        VectorIter args_iter = vector_iter(&function->args);
        indentation++;
        while(vector_iter_peek(&args_iter) != NULL) {
            AstFunArgNode* node = (AstFunArgNode*)vector_iter_next(&args_iter);
            char* name = parser_fmt_node(allocator, node->ident, 0);
            char* type = parser_fmt_node(allocator, node->type, 0);

            string_builder_indent(&builder, indentation);
            string_builder_write_format(&builder, "(%u) ", pos);
            string_builder_write_format(&builder, "%s: \"%s\"", type, name);
            string_builder_write_string(&builder, ",\n");

            pos++;
        }
        indentation--;

        if(function->args.len > 0) string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "],\n");
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".body = [");
        if(function->body.len > 0) string_builder_write_string(&builder, "\n");

        VectorIter body_iter = vector_iter(&function->body);
        indentation++;
        while(vector_iter_peek(&body_iter) != NULL) {
            AstNode* node = (AstNode*)vector_iter_next(&body_iter);
            char* formatted_node = parser_fmt_node(
                allocator,
                node,
                indentation
            );
            string_builder_write_string(&builder, formatted_node);
            string_builder_write_string(&builder, "\n");
        }
        indentation--;

        if(function->body.len > 0) string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "],\n");
        indentation--;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "},");

        break;
    }
    case AST_NODE_LET_BINDING: {
        AstLetNode* binding = (AstLetNode*)node;
        char* name = parser_fmt_node(allocator, binding->ident, indentation);
        char* type = parser_fmt_node(allocator, binding->type, indentation);
        char* value = parser_fmt_node(allocator, binding->value, indentation);

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "LetBinding{\n");
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, ".name = ");
        string_builder_write_format(&builder, "\"%s\",\n", name);
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_format(&builder, ".type = %s,\n", type);
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_format(&builder, ".value = %s,\n", value);
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "},");

        break;
    }
    case AST_NODE_FLOAT_LITERAL: {
        AstFloatNode* float_expr = (AstFloatNode*)node;
        string_builder_write_format(&builder, "%s", float_expr->value);
        break;
    }
    case AST_NODE_INT_LITERAL: {
        AstIntNode* int_expr = (AstIntNode*)node;
        string_builder_write_format(&builder, "%ld", int_expr->value);
        break;
    }
    case AST_NODE_UINT_LITERAL: {
        AstUintNode* uint_expr = (AstUintNode*)node;
        string_builder_write_format(&builder, "%lu", uint_expr->value);
        break;
    }
    case AST_NODE_BINARY_OP: {
        AstBinaryOpNode* binop = (AstBinaryOpNode*)node;
        string_builder_write_string(&builder, "AstBinaryOpNode{\n");
        indentation++;

        string_builder_indent(&builder, indentation);
        char* lhs = parser_fmt_node(allocator, binop->lhs, indentation);
        string_builder_write_format(&builder, ".lhs = \"%s\",\n", lhs);

        string_builder_indent(&builder, indentation);
        char* rhs = parser_fmt_node(allocator, binop->rhs, indentation);
        string_builder_write_format(&builder, ".rhs = \"%s\",\n", rhs);

        string_builder_indent(&builder, indentation);
        const char* op = lexer_fmt_token_kind(binop->op);
        string_builder_write_format(&builder, ".op = \"%s\",\n", op);
        indentation--;

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "}");

        break;
    }
    case AST_NODE_FUN_CALL: {
        AstFunCallNode* fun_call = (AstFunCallNode*)node;
        string_builder_write_string(&builder, "AstFunCallNode{\n");
        string_builder_indent(&builder, indentation + 2);

        char* name = parser_fmt_node(allocator, fun_call->ident, indentation);
        string_builder_write_format(&builder, ".name = \"%s\",\n", name);

        string_builder_indent(&builder, indentation + 2);
        string_builder_write_string(&builder, ".args = [");

        VectorIter args_iter = vector_iter(&fun_call->args);
        while(vector_iter_peek(&args_iter) != NULL) {
            AstNode* node = (AstNode*)vector_iter_next(&args_iter);
            char* formatted_node = parser_fmt_node(
                allocator,
                node,
                indentation + 2
            );
            string_builder_write_format(&builder, "%s", formatted_node);
            if(args_iter.cursor < fun_call->args.len) {
                string_builder_write_string(&builder, ", ");
            }
        }
        string_builder_write_string(&builder, "],\n");
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, "}");
        break;
    }
    case AST_NODE_TYPE_ANNOTATION: {
        AstTypeNode* type = (AstTypeNode*)node;
        string_builder_write_format(&builder, "%s", type->ident->name);
        break;
    }
    case AST_NODE_RETURN:
        break;
    }

    return string_builder_to_string(&builder);
}
