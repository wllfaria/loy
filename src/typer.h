#ifndef _TYPER_H
#define _TYPER_H

#include "collections/hash_map.h"
#include "parser.h"

typedef Vector TypedStatements;

typedef enum {
    TYPED_NODE_FUN,
    TYPED_NODE_FUN_ARG,
} TypedNodeKind;

typedef struct {
    TypedNodeKind kind;
} TypedNodeTag;

typedef struct {
    TypedNodeTag  tag;
    AstIdentNode* ident;
    AstTypeNode*  type;
} TypedFunArg;

typedef struct {
    TypedNodeTag tag;
} TypedAst;

typedef struct {
    TypedNodeTag    tag;
    AstIdentNode*   ident;
    TypedStatements args;
} FunctionDecl;

typedef struct {
    HashMap type_decl;
    HashMap functions;
} TyperContext;

// static void typer_typecheck_ast_node(AstNode* node);
// static void typer_typecheck_function(AstFunNode* node);

void typer_typecheck_ast(Ast ast);

#endif
