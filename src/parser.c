#include <stdio.h>

#include "parser.h"
#include "string/string_builder.h"

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
        char* formatted_token = lexer_fmt_token(needle, 0);
        printf("Invalid token, expected `%s` but found:\n%s\n", fmt,
               formatted_token);
        free(formatted_token);
        exit(EXIT_SUCCESS);
    }

    return needle;
}

static AstIdentNode* parser_parse_identifier(VectorIter* iter) {
    Token* identifier = assert_token_kind(iter, TOKEN_IDENT, "identifier");

    AstIdentNode* node = malloc(sizeof(AstIdentNode));

    if(node == NULL) {
        perror("Failed to allocate memory for AstIdentNode");
        exit(EXIT_FAILURE);
    }

    node->tag.kind = AST_NODE_IDENTIFIER;
    node->name     = identifier->lexeme;
    return node;
}

static AstTypeNode* parser_parse_type_annotation(VectorIter* iter) {
    assert_token_kind(iter, TOKEN_COLON, ":");
    AstIdentNode* ident      = parser_parse_identifier(iter);
    AstTypeNode*  annotation = malloc(sizeof(AstTypeNode));
    annotation->tag.kind = AST_NODE_TYPE_ANNOTATION;
    annotation->name     = ident;
    return annotation;
}

static AstNode* parser_parse_function_arg(VectorIter* iter) {
    AstIdentNode*  name = parser_parse_identifier(iter);
    AstTypeNode*   type = parser_parse_type_annotation(iter);
    AstFunArgNode* node = malloc(sizeof(AstFunArgNode));

    node->type     = type;
    node->ident    = name;
    node->tag.kind = AST_NODE_FUN_ARG;
    return (AstNode*)node;
}

// TODO(wiru): remove underscores from numbers
static AstNode* parser_parse_number_literal(VectorIter* iter) {
    Token* token = (Token*)vector_iter_next(iter);

    if(token->kind == TOKEN_INT) {
        if(token->is_signed) {
            i64 value        = strtoll(token->lexeme, NULL, 10);
            AstIntNode* expr = malloc(sizeof(AstIntNode));
            expr->tag.kind = AST_NODE_INT_LITERAL;
            expr->value    = value;
            return (AstNode*)expr;
        }

        u64 value         = strtoull(token->lexeme, NULL, 10);
        AstUintNode* expr = malloc(sizeof(AstUintNode));
        expr->tag.kind = AST_NODE_UINT_LITERAL;
        expr->value    = value;
        return (AstNode*)expr;
    }

    if(token->kind == TOKEN_FLOAT) {
        f64 value          = strtod(token->lexeme, NULL);
        AstFloatNode* expr = malloc(sizeof(AstFloatNode));
        expr->tag.kind = AST_NODE_FLOAT_LITERAL;
        expr->value    = value;
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
    VectorIter* iter,
    u8 precedence
);

static Statements parser_parse_function_call(VectorIter* iter) {
    Statements args = vector_create();

    Token* peeked = token_stream_peek_non_eof(iter, "idk");
    while(peeked != NULL && peeked->kind != TOKEN_RPAREN) {
        AstNode* arg = parser_parse_with_precedence(iter, BASE_PRECEDENCE);
        vector_push_ptr(&args, arg);
        Token* token = token_stream_peek_non_eof(iter, ",");
        // consume optional comma
        if(token->kind == TOKEN_COMMA) vector_iter_next(iter);
        peeked = vector_iter_peek(iter);
    }

    return args;
}

static AstNode* parser_parse_with_precedence(
    VectorIter* iter,
    u8 precedence
) {
    Token* token = (Token*)vector_iter_peek(iter);
    AstNode* lhs = NULL;

    switch(token->kind) {
    case TOKEN_INT: {
        lhs = parser_parse_number_literal(iter);
        break;
    }
    case TOKEN_FLOAT: {
        lhs = parser_parse_number_literal(iter);
        break;
    }
    case TOKEN_IDENT: {
        lhs = (AstNode*)parser_parse_identifier(iter);
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
            Statements call_args = parser_parse_function_call(iter);
            assert_token_kind(iter, TOKEN_RPAREN, ")");
            AstFunCallNode* call = malloc(sizeof(AstFunCallNode));
            call->tag.kind = AST_NODE_FUN_CALL;
            call->args     = call_args;
            call->name     = lhs;
            return (AstNode*)call;
        }
        default: break;
        }

        u8 op_precedence = token_operator_precedence(peeked, true);
        if(op_precedence > precedence) {
            break;
        }

        AstNode* rhs = parser_parse_with_precedence(iter,
                                                    op_precedence);
        AstBinaryOpNode* binop = malloc(sizeof(AstBinaryOpNode));
        binop->tag.kind = AST_NODE_BINARY_OP;
        binop->lhs      = lhs;
        binop->rhs      = rhs;
        lhs             = (AstNode*)binop;
    }

    return lhs;
}

static AstNode* parser_parse_expression(VectorIter* iter) {
    return parser_parse_with_precedence(iter, BASE_PRECEDENCE);
}

static AstNode* parser_parse_let_binding(VectorIter* iter) {
    assert_token_kind(iter, TOKEN_LET, "let");
    AstIdentNode* name = parser_parse_identifier(iter);
    AstTypeNode*  type = parser_parse_type_annotation(iter);
    assert_token_kind(iter, TOKEN_EQUAL, "=");
    AstNode* value = parser_parse_expression(iter);
    assert_token_kind(iter, TOKEN_SEMI, ";");

    AstLetNode* node = malloc(sizeof(AstLetNode));
    node->name     = name;
    node->type     = type;
    node->tag.kind = AST_NODE_LET_BINDING;
    node->value    = value;
    return (AstNode*)node;
}

static Statements parser_parse_block(VectorIter* iter) {
    Token* peeked = (Token*)vector_iter_peek(iter);
    Vector block  = vector_create();

    while(peeked != NULL && peeked->kind != TOKEN_RBRACE) {
        switch(peeked->kind) {
        case TOKEN_LET: {
            AstNode* let_binding = parser_parse_let_binding(iter);
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

static AstFunNode* parser_parse_function(VectorIter* iter) {
    assert_token_kind(iter, TOKEN_FUN, "fun");
    AstIdentNode* identifier = parser_parse_identifier(iter);
    assert_token_kind(iter, TOKEN_LPAREN, "(");
    Statements args = vector_create();

    Token* peeked = vector_iter_peek(iter);
    while(peeked != NULL && peeked->kind != TOKEN_RPAREN) {
        AstNode* arg = parser_parse_function_arg(iter);
        vector_push_ptr(&args, arg);
        peeked = vector_iter_peek(iter);
    }

    assert_token_kind(iter, TOKEN_RPAREN, ")");
    assert_token_kind(iter, TOKEN_THIN_ARROW, "->");

    AstIdentNode* return_type = parser_parse_identifier(iter);

    assert_token_kind(iter, TOKEN_LBRACE, "{");
    Statements block = parser_parse_block(iter);
    assert_token_kind(iter, TOKEN_RBRACE, "}");

    AstFunNode* function = malloc(sizeof(AstFunNode));
    function->tag.kind    = AST_NODE_FUN;
    function->name        = (AstIdentNode*)identifier;
    function->args        = args;
    function->return_type = return_type;
    function->body        = block;

    return function;
}

Ast parser_parse_token_stream(TokenStream* stream) {
    Ast ast         = { .statements = vector_create() };
    VectorIter iter = vector_iter(&stream->tokens);

    while(vector_iter_peek(&iter) != NULL) {
        Token* token = (Token*)vector_iter_peek(&iter);

        switch(token->kind) {
        case TOKEN_FUN: {
            AstNode* function = (AstNode*)parser_parse_function(&iter);
            vector_push_ptr(&ast.statements, function);
            return ast;
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

static void parser_destroy_statement(void* item);

static void parser_destroy_node(AstNode* node) {
    switch(node->kind) {
    case AST_NODE_IDENTIFIER: {
        break;
    }
    case AST_NODE_FUN: {
        AstFunNode* function = (AstFunNode*)node;
        parser_destroy_node((AstNode*)function->name);
        parser_destroy_node((AstNode*)function->return_type);
        vector_destroy(&function->args, parser_destroy_statement);
        vector_destroy(&function->body, parser_destroy_statement);
        break;
    }
    case AST_NODE_FUN_ARG: {
        AstFunArgNode* arg = (AstFunArgNode*)node;
        parser_destroy_node((AstNode*)arg->ident);
        parser_destroy_node((AstNode*)arg->type);
        break;
    }
    case AST_NODE_LET_BINDING: {
        AstLetNode* binding = (AstLetNode*)node;
        parser_destroy_node((AstNode*)binding->name);
        parser_destroy_node((AstNode*)binding->type);
        parser_destroy_node(binding->value);
        break;
    }
    case AST_NODE_INT_LITERAL:
    case AST_NODE_UINT_LITERAL:
    case AST_NODE_FLOAT_LITERAL:
        break;
    case AST_NODE_BINARY_OP:
        break;
    case AST_NODE_FUN_CALL: {
        AstFunCallNode* binding = (AstFunCallNode*)node;
        parser_destroy_node(binding->name);
        vector_destroy(&binding->args, parser_destroy_statement);
        break;
    }
    case AST_NODE_TYPE_ANNOTATION:
        break;
    }
    free(node);
}

static void parser_destroy_statement(void* item) {
    AstNode* node = *(void**)item;
    parser_destroy_node(node);
}

void parser_destroy(Ast* ast) {
    vector_destroy(&ast->statements, parser_destroy_statement);
}

char* parser_fmt_node(void* item, u64 indentation) {
    AstNode* node         = (AstNode*)item;
    StringBuilder builder = string_builder_create();

    switch(node->kind) {
    case AST_NODE_IDENTIFIER: {
        AstIdentNode* identifier = (AstIdentNode*)node;
        string_builder_write_format(&builder, "%s", identifier->name);
        break;
    }
    case AST_NODE_FUN_ARG: {
        AstFunArgNode* arg = (AstFunArgNode*)node;
        char* name         = parser_fmt_node(arg->ident, indentation);
        char* type         = parser_fmt_node(arg->type, indentation);

        string_builder_write_format(&builder, "%s: %s", name, type);
        free(name);
        free(type);
        break;
    }
    case AST_NODE_FUN: {
        AstFunNode* function = (AstFunNode*)node;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "AstFunNode{\n");

        char* name        = parser_fmt_node(function->name, 0);
        char* return_type = parser_fmt_node(function->return_type, 0);

        string_builder_indent(&builder, indentation + 1);
        string_builder_write_format(&builder, ".name = \"%s\",\n", name);
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, ".return type = ");
        string_builder_write_format(&builder, "\"%s\"\n", return_type);
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, ".args = [\n");

        free(name);
        free(return_type);

        u8 pos = 0;
        VectorIter args_iter = vector_iter(&function->args);
        while(vector_iter_peek(&args_iter) != NULL) {
            AstFunArgNode* node = *(void**)vector_iter_next(&args_iter);
            char* name          = parser_fmt_node(node->ident, 0);
            char* type          = parser_fmt_node(node->type, 0);

            string_builder_indent(&builder, indentation + 2);
            string_builder_write_format(&builder, "(%u) ", pos);
            string_builder_write_format(&builder, "%s: \"%s\"", type, name);
            string_builder_write_string(&builder, ",\n");

            pos++;
            free(name);
            free(type);
        }

        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, "],\n");
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, ".body = [\n");

        VectorIter body_iter = vector_iter(&function->body);
        while(vector_iter_peek(&body_iter) != NULL) {
            AstNode* node        = *(void**)vector_iter_next(&body_iter);
            char* formatted_node = parser_fmt_node(node, indentation + 2);
            string_builder_write_string(&builder, formatted_node);
            string_builder_write_string(&builder, "\n");

            free(formatted_node);
        }

        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, "],\n");
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "},");

        break;
    }
    case AST_NODE_LET_BINDING: {
        AstLetNode* binding = (AstLetNode*)node;
        char* name          = parser_fmt_node(binding->name, indentation);
        char* type          = parser_fmt_node(binding->type, indentation);
        char* value         = parser_fmt_node(binding->value, indentation);

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

        free(name);
        free(type);
        free(value);

        break;
    }
    case AST_NODE_FLOAT_LITERAL: {
        AstFloatNode* float_expr = (AstFloatNode*)node;
        string_builder_write_format(&builder, "%lf", float_expr->value);
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

        char* name = parser_fmt_node(fun_call->name, indentation);
        string_builder_write_format(&builder, ".name = \"%s\",\n", name);
        free(name);

        string_builder_indent(&builder, indentation + 2);
        string_builder_write_string(&builder, ".args = [");

        VectorIter args_iter = vector_iter(&fun_call->args);
        while(vector_iter_peek(&args_iter) != NULL) {
            AstNode* node        = *(void**)vector_iter_next(&args_iter);
            char* formatted_node = parser_fmt_node(node, indentation + 2);
            string_builder_write_format(&builder, "%s", formatted_node);
            if(args_iter.cursor < fun_call->args.len) {
                string_builder_write_string(&builder, ", ");
            }
            free(formatted_node);
        }
        string_builder_write_string(&builder, "],\n");
        string_builder_indent(&builder, indentation + 1);
        string_builder_write_string(&builder, "}");
        break;
    }
    case AST_NODE_TYPE_ANNOTATION:
        break;
    }

    return string_builder_to_string(&builder);
}
