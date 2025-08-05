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

Token* token_stream_peek_non_eof(VectorIter* iter, const char* expected) {
    Token* peeked = vector_iter_peek(iter);
    if(peeked == NULL) {
        fprintf(stderr, "Invalid token, expected %s but found `EOF`", expected);
        exit(EXIT_SUCCESS);
    }

    return peeked;
}

static void parser_parse_optional_comma(VectorIter* iter) {
    Token* token = token_stream_peek_non_eof(iter, ",");
    if(token->kind == TOKEN_COMMA) vector_iter_next(iter);
}

static Token* assert_token_kind(
    VectorIter* iter,
    TokenKind expected,
    char* fmt
) {
    Token* needle = (Token*)vector_iter_next(iter);
    if(needle == NULL) {
        printf("Invalid token, expected `%s` but found `EOF`\n", fmt);
        exit(EXIT_SUCCESS);
    }

    if(needle->kind != expected) {
        Allocator allocator = generic_allocator_create();
        char* formatted_token = lexer_fmt_token(&allocator, needle, 0);
        printf(
            "Invalid token, expected `%s` but found:\n%s\n",
            fmt,
            formatted_token
        );
        allocator.free(allocator.ctx, formatted_token);
        exit(EXIT_SUCCESS);
    }

    return needle;
}

static AstIdentNode* parser_parse_identifier(
    Allocator* allocator,
    VectorIter* iter
) {
    Token* identifier = assert_token_kind(iter, TOKEN_IDENT, "identifier");
    AstIdentNode* node = allocator->alloc(allocator->ctx, sizeof(AstIdentNode));
    node->tag.kind = AST_NODE_IDENTIFIER;
    node->name = identifier->lexeme;
    return node;
}

static AstTypeNode* parser_parse_type_annotation(
    Allocator* allocator,
    VectorIter* iter
) {
    assert_token_kind(iter, TOKEN_COLON, ":");
    AstIdentNode* ident = parser_parse_identifier(allocator, iter);
    AstTypeNode* annotation = allocator->alloc(
        allocator->ctx,
        sizeof(AstTypeNode)
    );
    annotation->tag.kind = AST_NODE_TYPE_ANNOTATION;
    annotation->ident = ident;
    return annotation;
}

static AstFunArgNode* parser_parse_function_arg(
    Allocator* allocator,
    VectorIter* iter
) {
    AstIdentNode* name = parser_parse_identifier(allocator, iter);
    AstTypeNode* type = parser_parse_type_annotation(allocator, iter);
    parser_parse_optional_comma(iter);
    AstFunArgNode* node = allocator->alloc(
        allocator->ctx,
        sizeof(AstFunArgNode)
    );
    node->type = type;
    node->ident = name;
    node->tag.kind = AST_NODE_FUN_ARG;
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
            AstIntNode* expr = allocator->alloc(
                allocator->ctx,
                sizeof(AstIntNode)
            );
            expr->tag.kind = AST_NODE_INT_LITERAL;
            expr->value = value;
            return (AstNode*)expr;
        }

        u64 value = strtoull(token->lexeme, NULL, 10);
        AstUintNode* expr = allocator->alloc(
            allocator->ctx,
            sizeof(AstUintNode)
        );
        expr->tag.kind = AST_NODE_UINT_LITERAL;
        expr->value = value;
        return (AstNode*)expr;
    }

    if(token->kind == TOKEN_FLOAT) {
        AstFloatNode* expr = allocator->alloc(
            allocator->ctx,
            sizeof(AstFloatNode)
        );
        expr->tag.kind = AST_NODE_FLOAT_LITERAL;
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
        return false;
    }
    return false;
}

static AstNode* parser_parse_with_precedence(
    Allocator* allocator,
    VectorIter* iter,
    u8 precedence
);

static Statements parser_parse_function_call(
    Allocator* allocator,
    VectorIter* iter
) {
    Statements args = vector_create(allocator);

    Token* peeked = token_stream_peek_non_eof(iter, "idk");
    while(peeked != NULL && peeked->kind != TOKEN_RPAREN) {
        AstNode* arg = parser_parse_with_precedence(
            allocator,
            iter,
            BASE_PRECEDENCE
        );
        vector_push_ptr(&args, arg);
        parser_parse_optional_comma(iter);
        peeked = vector_iter_peek(iter);
    }

    return args;
}

static AstNode* parser_parse_with_precedence(
    Allocator* allocator,
    VectorIter* iter,
    u8 precedence
) {
    Token* token = (Token*)vector_iter_peek(iter);
    AstNode* lhs = NULL;

    switch(token->kind) {
    case TOKEN_INT: {
        lhs = parser_parse_number_literal(allocator, iter);
        break;
    }
    case TOKEN_FLOAT: {
        lhs = parser_parse_number_literal(allocator, iter);
        break;
    }
    case TOKEN_IDENT: {
        lhs = (AstNode*)parser_parse_identifier(allocator, iter);
        break;
    }
    case TOKEN_EQUAL:
    case TOKEN_COLON:
    case TOKEN_ASSIGN_ADD:
    case TOKEN_LET:
    case TOKEN_FUN:
    case TOKEN_SEMI:
    case TOKEN_LPAREN:
    case TOKEN_RPAREN:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
    case TOKEN_THIN_ARROW:
    case TOKEN_PLUS:
    case TOKEN_COMMA:
    case TOKEN_PLUS_PLUS:
        break;
    }

    for(;;) {
        Token* peeked = token_stream_peek_non_eof(iter, "an operator");
        if(!token_is_operator(peeked)) {
            break;
        }

        switch(peeked->kind) {
        case TOKEN_LPAREN: {
            assert_token_kind(iter, TOKEN_LPAREN, "(");
            Statements call_args = parser_parse_function_call(allocator, iter);
            assert_token_kind(iter, TOKEN_RPAREN, ")");
            AstFunCallNode* call = allocator->alloc(
                allocator->ctx,
                sizeof(AstFunCallNode)
            );
            call->tag.kind = AST_NODE_FUN_CALL;
            call->args = call_args;
            call->ident = (AstIdentNode*)lhs;
            return (AstNode*)call;
        }
        default: break;
        }

        u8 op_precedence = token_operator_precedence(peeked, true);
        if(op_precedence > precedence) {
            break;
        }

        AstNode* rhs = parser_parse_with_precedence(
            allocator,
            iter,
            op_precedence
        );
        AstBinaryOpNode* binop = allocator->alloc(
            allocator->ctx,
            sizeof(AstBinaryOpNode)
        );
        binop->tag.kind = AST_NODE_BINARY_OP;
        binop->lhs = lhs;
        binop->rhs = rhs;
        lhs = (AstNode*)binop;
    }

    return lhs;
}

static AstNode* parser_parse_expression(
    Allocator* allocator,
    VectorIter* iter
) {
    return parser_parse_with_precedence(allocator, iter, BASE_PRECEDENCE);
}

static AstNode* parser_parse_let_binding(
    Allocator* allocator,
    VectorIter* iter
) {
    assert_token_kind(iter, TOKEN_LET, "let");
    AstIdentNode* name = parser_parse_identifier(allocator, iter);
    AstTypeNode* type = parser_parse_type_annotation(allocator, iter);
    assert_token_kind(iter, TOKEN_EQUAL, "=");
    AstNode* value = parser_parse_expression(allocator, iter);
    assert_token_kind(iter, TOKEN_SEMI, ";");

    AstLetNode* node = allocator->alloc(
        allocator->ctx,
        sizeof(AstLetNode)
    );
    node->ident = name;
    node->type = type;
    node->tag.kind = AST_NODE_LET_BINDING;
    node->value = value;
    return (AstNode*)node;
}

static Statements parser_parse_block(
    Allocator* allocator,
    VectorIter* iter
) {
    Token* peeked = (Token*)vector_iter_peek(iter);
    Vector block = vector_create(allocator);

    while(peeked != NULL && peeked->kind != TOKEN_RBRACE) {
        switch(peeked->kind) {
        case TOKEN_LET: {
            AstNode* let_binding = parser_parse_let_binding(allocator, iter);
            vector_push_ptr(&block, let_binding);
            break;
        }
        case TOKEN_EQUAL:
        case TOKEN_COLON:
        case TOKEN_ASSIGN_ADD:
        case TOKEN_IDENT:
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
            break;
        }

        peeked = (Token*)vector_iter_peek(iter);
    }

    return block;
}

static AstFunNode* parser_parse_function(
    Allocator* allocator,
    VectorIter* iter
) {
    assert_token_kind(iter, TOKEN_FUN, "fun");
    AstIdentNode* identifier = parser_parse_identifier(allocator, iter);
    assert_token_kind(iter, TOKEN_LPAREN, "(");
    Statements args = vector_create(allocator);

    Token* peeked = vector_iter_peek(iter);
    while(peeked != NULL && peeked->kind != TOKEN_RPAREN) {
        AstFunArgNode* arg = parser_parse_function_arg(allocator, iter);
        vector_push_ptr(&args, arg);
        peeked = vector_iter_peek(iter);
    }

    assert_token_kind(iter, TOKEN_RPAREN, ")");
    assert_token_kind(iter, TOKEN_THIN_ARROW, "->");

    AstIdentNode* return_type = parser_parse_identifier(allocator, iter);

    assert_token_kind(iter, TOKEN_LBRACE, "{");
    Statements block = parser_parse_block(allocator, iter);
    assert_token_kind(iter, TOKEN_RBRACE, "}");

    AstFunNode* function = allocator->alloc(
        allocator->ctx,
        sizeof(AstFunNode)
    );
    function->tag.kind = AST_NODE_FUN;
    function->ident = (AstIdentNode*)identifier;
    function->args = args;
    function->return_type = return_type;
    function->body = block;

    return function;
}

Ast parser_parse_token_stream(
    Allocator* allocator,
    TokenStream* stream,
    StringSlice file
) {
    (void)file;
    Ast ast = { .statements = vector_create(allocator) };
    VectorIter iter = vector_iter(&stream->tokens);

    while(vector_iter_peek(&iter) != NULL) {
        Token* token = (Token*)vector_iter_peek(&iter);

        switch(token->kind) {
        case TOKEN_FUN: {
            AstNode* function = (AstNode*)parser_parse_function(
                allocator,
                &iter
            );
            vector_push_ptr(&ast.statements, function);
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
        }
    }

    return ast;
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
    }

    return string_builder_to_string(&builder);
}
