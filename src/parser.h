#ifndef _PARSER_H
#define _PARSER_H

#include "lexer.h"

typedef Vector Statements;

typedef enum {
    AST_NODE_IDENTIFIER,
    AST_NODE_FUN,
    AST_NODE_FUN_ARG,
    AST_NODE_LET_BINDING,
    AST_NODE_BINARY_OP,
    AST_NODE_TYPE_ANNOTATION,

    AST_NODE_FUN_CALL,
    AST_NODE_INT_LITERAL,
    AST_NODE_UINT_LITERAL,
    AST_NODE_FLOAT_LITERAL,
} AstNodeKind;

typedef struct {
    AstNodeKind kind;
} AstNodeTag;

typedef AstNodeTag AstNode;

typedef struct {
    AstNodeTag tag;
    char*      name;
} AstIdentNode;

typedef struct {
    AstNodeTag    tag;
    AstIdentNode* name;
} AstTypeNode;

typedef struct {
    AstNodeTag    tag;
    AstIdentNode* name;
    AstTypeNode*  type;
    AstNode*      value;
} AstLetNode;

typedef struct {
    AstNodeTag    tag;
    AstIdentNode* ident;
    AstTypeNode*  type;
} AstFunArgNode;

typedef struct {
    AstNodeTag    tag;
    AstIdentNode* name;
    Statements    args;
    AstIdentNode* return_type;
    Statements    body;
} AstFunNode;

typedef struct {
    AstNodeTag tag;
    AstNode*   name;
    Statements args;
} AstFunCallNode;

typedef struct {
    AstNodeTag tag;
    i64        value;
} AstIntNode;

typedef struct {
    AstNodeTag tag;
    u64        value;
} AstUintNode;

typedef struct {
    AstNodeTag tag;
    f64        value;
} AstFloatNode;

typedef struct {
    AstNodeTag tag;
    AstNode*   lhs;
    AstNode*   rhs;
} AstBinaryOpNode;

typedef struct {
    Statements statements;
} Ast;

Ast parser_parse_token_stream(TokenStream* stream);
char* parser_fmt_node(void* node, u64 indentation);
void parser_destroy(Ast* ast);

#endif
