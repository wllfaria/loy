#ifndef _TYPER_H
#define _TYPER_H

#include "parser.h"

typedef struct {
    int a;
} TyperContext;

// static void typer_typecheck_ast_node(AstNode* node);
// static void typer_typecheck_function(AstFunNode* node);

void typer_typecheck_ast(Ast ast);

#endif
