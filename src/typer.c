#include <stdio.h>

#include "typer.h"
#include "collections/vector.h"

static const char* primitives[] = {
    "usize",
};

static const u64 primitive_count = ARRAY_LEN(primitives);

static void typer_collect_type_decl(TyperContext* ctx, Ast* ast) {
    for(u64 i = 0; i < primitive_count; i++) {
        char* primitive = (char*)primitives[i];

        hash_map_insert(
            &ctx->type_decl,
            primitive,
            strlen(primitive),
            primitive
        );
    }
}

static TypedStatements typer_collect_function_args(
    TyperContext* ctx,
    AstFunNode* fun
) {
    Vector typed_args = vector_create();
    VectorIter iter   = vector_iter(&fun->args);

    while(vector_iter_peek(&iter) != NULL) {
        AstFunArgNode* arg = (AstFunArgNode*)vector_iter_next(&iter);

        const char* type_name = hash_map_get(
            &ctx->type_decl,
            arg->type->name->name,
            strlen(arg->type->name->name)
        );

        if(type_name == NULL) {
            fprintf(stderr, "UNDECLARED TYPE\n");
            exit(EXIT_FAILURE);
        }

        TypedFunArg typed_arg = {
            .tag   = { .kind = TYPED_NODE_FUN_ARG },
            .ident = arg->ident,
            .type  = arg->type,
        };

        vector_push(&typed_args, typed_arg);
    }

    return typed_args;
}

static void typer_collect_functions(TyperContext* ctx, Ast* ast) {
    VectorIter iter = vector_iter(&ast->statements);

    while(vector_iter_peek(&iter) != NULL) {
        AstNode* node = *(void**)vector_iter_next(&iter);

        if(node->kind == AST_NODE_FUN) {
            AstFunNode* fun = (AstFunNode*)node;

            TypedStatements args = typer_collect_function_args(ctx, fun);

            FunctionDecl* fun_decl = malloc(sizeof(FunctionDecl));
            fun_decl->tag.kind = TYPED_NODE_FUN;
            fun_decl->ident    = fun->name;
            fun_decl->args     = args;

            hash_map_insert(
                &ctx->functions,
                fun_decl->ident->name,
                strlen(fun_decl->ident->name),
                fun_decl
            );
        }
    }
}

void typer_typecheck_ast(Ast ast) {
    TyperContext ctx = {
        .type_decl = hash_map_create(),
        .functions = hash_map_create(),
    };

    typer_collect_type_decl(&ctx, &ast);
    typer_collect_functions(&ctx, &ast);
}
