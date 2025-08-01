#include "typer.h"
#include "collections/vector.h"

// static void typer_typecheck_ast_node(AstNode* node) {
// switch(node->kind) {
// case AST_NODE_FUN:
//     break;
// // typer_typecheck_function((AstFunNode*)node);
// case AST_NODE_IDENTIFIER:
// case AST_NODE_FUN_ARG:
// case AST_NODE_LET_BINDING:
// case AST_NODE_BINARY_OP:
// case AST_NODE_FUN_CALL:
// case AST_NODE_INT_LITERAL:
// case AST_NODE_UINT_LITERAL:
// case AST_NODE_FLOAT_LITERAL:
//     break;
// }
// }

void typer_typecheck_ast(Ast ast) {
    (void)ast;
    // VectorIter iter = vector_iter(&ast.statements);

    // while(vector_iter_peek(&iter) != NULL) {
    //     AstNode* node = *(void**)vector_iter_next(&iter);
    //     typer_typecheck_ast_node(node);
    // }
}
