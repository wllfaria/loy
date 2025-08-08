#include <math.h>
#include <stdint.h>
#include <stdio.h>

#include "typer.h"
#include "collections/vector.h"
#include "string/string_builder.h"

static TypeInvalid invalid_type = { .kind = TYPE_INVALID };

static TypeFloat type_f32 = { .kind = TYPE_FLOAT, .bit_width = 32 };
static TypeFloat type_f64 = { .kind = TYPE_FLOAT, .bit_width = 64 };

static TypeInt type_usize = {
    .kind = TYPE_USIZE,
    .bit_width = 64,
    .is_signed = false,
};

static TypeInt type_isize = {
    .kind = TYPE_ISIZE,
    .bit_width = 64,
    .is_signed = true,
};

static TypeInt type_i8 = {
    .kind = TYPE_INT,
    .bit_width = 8,
    .is_signed = true,
};

static TypeInt type_i16 = {
    .kind = TYPE_INT,
    .bit_width = 16,
    .is_signed = true,
};

static TypeInt type_i32 = {
    .kind = TYPE_INT,
    .bit_width = 32,
    .is_signed = true,
};

static TypeInt type_i64 = {
    .kind = TYPE_INT,
    .bit_width = 64,
    .is_signed = true,
};

static TypeInt type_u8 = {
    .kind = TYPE_INT,
    .bit_width = 8,
    .is_signed = false,
};

static TypeInt type_u16 = {
    .kind = TYPE_INT,
    .bit_width = 16,
    .is_signed = false,
};

static TypeInt type_u32 = {
    .kind = TYPE_INT,
    .bit_width = 32,
    .is_signed = false,
};

static TypeInt type_u64 = {
    .kind = TYPE_INT,
    .bit_width = 64,
    .is_signed = false,
};

static TypeVoid type_void = { .kind = TYPE_VOID };

static PrimitiveTypeEntry primitives[] = {
    { .name = "void", .type = (Type*)&type_void },
    { .name = "f32", .type = (Type*)&type_f32 },
    { .name = "f64", .type = (Type*)&type_f64 },
    { .name = "usize", .type = (Type*)&type_usize },
    { .name = "isize", .type = (Type*)&type_isize },
    { .name = "i8", .type = (Type*)&type_i8 },
    { .name = "i16", .type = (Type*)&type_i16 },
    { .name = "i32", .type = (Type*)&type_i32 },
    { .name = "i64", .type = (Type*)&type_i64 },
    { .name = "u8", .type = (Type*)&type_u8 },
    { .name = "u16", .type = (Type*)&type_u16 },
    { .name = "u32", .type = (Type*)&type_u32 },
    { .name = "u64", .type = (Type*)&type_u64 },
};

static const u64 primitive_count = ARRAY_LEN(primitives);

static char* format_type(Allocator* allocator, Type* type) {
    StringBuilder builder = string_builder_create(allocator);

    switch(type->kind) {
    case TYPE_INVALID: {
        string_builder_write_string(&builder, "unknown");
        break;
    }
    case TYPE_FLOAT: {
        TypeFloat* f = (TypeFloat*)type;
        string_builder_write_format(&builder, "f%d", f->bit_width);
        break;
    }
    case TYPE_USIZE: {
        string_builder_write_string(&builder, "usize");
        break;
    }
    case TYPE_ISIZE: {
        string_builder_write_string(&builder, "isize");
        break;
    }
    case TYPE_INT: {
        TypeInt* i = (TypeInt*)type;

        if(i->is_signed) {
            string_builder_write_format(&builder, "i%d", i->bit_width);
        } else {
            string_builder_write_format(&builder, "u%d", i->bit_width);
        }

        break;
    }
    case TYPE_VOID:
        string_builder_write_string(&builder, "void");
        break;
    }

    return string_builder_to_string(&builder);
}

char* typer_fmt_node(Allocator* allocator, void* item, u64 indentation) {
    TypedNode* node = (TypedNode*)item;
    StringBuilder builder = string_builder_create(allocator);

    switch(node->tag.kind) {
    case TYPED_NODE_FUN: {
        TypedFunction* fun = (TypedFunction*)node;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "TypedFunction{\n");
        indentation++;

        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, ".ident = \"%s\",\n", fun->ident);

        string_builder_indent(&builder, indentation);
        char* return_type = format_type(allocator, fun->return_type);
        string_builder_write_format(&builder, ".return_type = \"%s\",\n", return_type);

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".args = [");
        if(fun->args.len > 0) string_builder_write_string(&builder, "\n");

        indentation++;
        VectorIter args_iter = vector_iter(&fun->args);
        u8 pos = 0;
        while(vector_iter_peek(&args_iter) != NULL) {
            TypedFunArg* arg = (TypedFunArg*)vector_iter_next(&args_iter);

            string_builder_indent(&builder, indentation);
            string_builder_write_format(&builder, "(%u) ", pos);

            char* type = format_type(allocator, arg->type);
            string_builder_write_format(&builder, "%s: ", type);

            string_builder_write_format(&builder, "\"%s\"", arg->ident);
            string_builder_write_string(&builder, ",\n");
            pos++;
        }

        indentation--;
        if(fun->args.len > 0) string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "],\n");

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".body = TypedBlock{\n");
        indentation++;

        string_builder_indent(&builder, indentation);
        char* block_return_type = format_type(allocator, fun->body.return_type);
        string_builder_write_format(&builder, ".return_type = \"%s\",\n", block_return_type);

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".items = [");
        if(fun->body.block.len > 0) string_builder_write_string(&builder, "\n");
        indentation++;

        VectorIter body_iter = vector_iter(&fun->body.block);
        while(vector_iter_peek(&body_iter) != NULL) {
            TypedNode* statement = (TypedNode*)vector_iter_next(&body_iter);
            char* statement_fmt = typer_fmt_node(allocator, statement, indentation);
            string_builder_write_string(&builder, statement_fmt);
        }
        indentation--;
        if(fun->body.block.len > 0) string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "],\n");

        indentation--;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "}\n");

        indentation--;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "}");
        break;
    }
    case TYPED_NODE_FLOAT_LITERAL: {
        TypedFloat* float_node = (TypedFloat*)node;
        string_builder_write_format(&builder, "%lf", float_node->value);
        break;
    }
    case TYPED_NODE_INT_LITERAL: {
        TypedInt* int_node = (TypedInt*)node;
        string_builder_write_format(&builder, "%ld", int_node->value);
        break;
    }
    case TYPED_NODE_UINT_LITERAL: {
        TypedUint* uint_node = (TypedUint*)node;
        string_builder_write_format(&builder, "%lu", uint_node->value);
        break;
    }
    case TYPED_NODE_LET_BINDING: {
        TypedLetBinding* binding = (TypedLetBinding*)node;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "TypedLetBinding{\n");
        indentation++;

        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, ".name = \"%s\",\n", binding->ident);

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".type = ");
        char* type = format_type(allocator, binding->type);
        string_builder_write_format(&builder, "%s,\n", type);

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, ".value = ");
        char* value_fmt = typer_fmt_node(allocator, binding->value, indentation);
        string_builder_write_format(&builder, "%s,\n", value_fmt);

        indentation--;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "},\n");
        break;
    }
    case TYPED_NODE_FUN_CALL: {
        TypedFunCall* fun_call = (TypedFunCall*)node;

        string_builder_write_string(&builder, "TypedFunCall{\n");
        indentation++;

        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, ".ident = \"%s\"\n", fun_call->ident);

        char* type = format_type(allocator, fun_call->type);
        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, ".type = \"%s\"\n", type);

        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, ".args = [\n", fun_call->ident);
        indentation++;
        VectorIter args_iter = vector_iter(&fun_call->args);
        u8 pos = 0;
        while(vector_iter_peek(&args_iter) != NULL) {
            TypedNode* arg = (TypedNode*)vector_iter_next(&args_iter);

            string_builder_indent(&builder, indentation);
            string_builder_write_format(&builder, "(%u) ", pos);

            char* arg_fmt = typer_fmt_node(allocator, arg, indentation);
            string_builder_write_format(&builder, "%s,\n", arg_fmt);
            pos++;
        }
        indentation--;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "],\n");

        indentation--;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "}");
        break;
    }
    case TYPED_NODE_IDENT: {
        TypedIdent* ident = (TypedIdent*)node;
        char* type = format_type(allocator, ident->type);
        string_builder_write_format(&builder, "\"%s\"", type);
        break;
    }
    case TYPED_NODE_RETURN: {
        TypedReturn* typed_return = (TypedReturn*)node;
        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "TypedReturn{\n");
        indentation++;

        string_builder_indent(&builder, indentation);
        char* return_value = typer_fmt_node(allocator, typed_return->return_value, indentation);
        string_builder_write_format(&builder, ".return_value = \"%s\"\n", return_value);

        string_builder_indent(&builder, indentation);
        char* return_type = format_type(allocator, typed_return->return_type);
        string_builder_write_format(&builder, ".type = \"%s\"\n", return_type);
        indentation--;

        string_builder_indent(&builder, indentation);
        string_builder_write_string(&builder, "},\n");

        break;
    }
    case TYPED_NODE_FUN_ARG: {
        break;
    }
    }

    return string_builder_to_string(&builder);
}

static Scope* typer_scope_get_parent(TyperContext* ctx, Scope* scope) {
    return vector_get(&ctx->scopes, (u64)scope->parent);
}

static Scope* typer_scope_get_current(TyperContext* ctx) {
    return vector_get_last(&ctx->scopes);
}

static void typer_scope_add_symbol(TyperContext* ctx, char* name, Type* type) {
    Scope* curr_scope = typer_scope_get_current(ctx);
    hash_map_insert(&curr_scope->symbol_table, name, strlen(name), type);
}

static Type* typer_scope_get_symbol(TyperContext* ctx, char* name) {
    Scope* curr_scope = typer_scope_get_current(ctx);

    while(curr_scope->parent != -1) {
        Type* type = hash_map_get(&curr_scope->symbol_table, name, strlen(name));
        if(type != NULL) return type;
        curr_scope = typer_scope_get_parent(ctx, curr_scope);
    }

    return NULL;
}

static Scope* typer_scope_push(Allocator* allocator, TyperContext* ctx) {
    i64 parent = (i64)ctx->scopes.len - 1;
    Scope new_scope = {
        .symbol_table = hash_map_create(allocator),
        .parent = parent,
    };
    vector_push(&ctx->scopes, new_scope);
    return vector_get_last(&ctx->scopes);
}

static void typer_collect_type_decl(TyperContext* ctx, Ast* ast) {
    (void)ast;

    for(u64 i = 0; i < primitive_count; i++) {
        const char* name = primitives[i].name;
        Type* type = primitives[i].type;
        hash_map_insert(&ctx->type_decl, (char*)name, strlen(name), type);
    }
}

static Type* typer_assert_type_declaration(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstNode* node,
    char* name
) {
    Type* type = (Type*)hash_map_get(&ctx->type_decl, name, strlen(name));
    if(type == NULL) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "Undeclared type `%s`", name);

        Span span = {
            .file = compile_ctx->unit.content,
            .label = string_builder_to_string(&builder),
            .info = "maybe declare it lol",
            .offset = node->byte_offset,
        };

        error_report_push_span(&compile_ctx->report, span);

        return NULL;
    }

    return type;
}

static LoyResult typer_collect_function_args(
    TypedStatements* out,
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstFunNode* fun
) {
    VectorIter iter = vector_iter(&fun->args);

    while(vector_iter_peek(&iter) != NULL) {
        AstFunArgNode* arg = (AstFunArgNode*)vector_iter_next(&iter);
        char* type_name = arg->type->ident->name;
        Type* arg_type = typer_assert_type_declaration(
            allocator,
            compile_ctx,
            ctx,
            (AstNode*)arg,
            type_name
        );
        if(arg_type == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

        TypedFunArg typed_arg = {
            .tag = { .kind = TYPED_NODE_FUN_ARG },
            .ident = arg->ident->name,
            .type = arg_type,
        };

        vector_push(out, typed_arg);
    }

    return LOY_OK;
}

static LoyResult typer_collect_functions(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    Ast* ast
) {
    VectorIter iter = vector_iter(&ast->statements);
    while(vector_iter_peek(&iter) != NULL) {
        AstNode* node = (AstNode*)vector_iter_next(&iter);

        if(node->kind == AST_NODE_FUN) {
            AstFunNode* fun = (AstFunNode*)node;
            TypedStatements args = vector_create(allocator);
            if(typer_collect_function_args(&args, allocator, compile_ctx, ctx, fun) != LOY_OK) {
                return LOY_ERROR_PARSER_INVALID_TOKEN;
            }

            Type* return_type = typer_assert_type_declaration(
                allocator,
                compile_ctx,
                ctx,
                (AstNode*)fun->return_type,
                fun->return_type->name
            );
            if(return_type == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

            FunctionDecl* fun_decl = allocator->alloc(allocator->ctx, sizeof(FunctionDecl));
            if(fun_decl == NULL) return LOY_ERROR_PARSER_INVALID_TOKEN;

            fun_decl->tag.kind = TYPED_NODE_FUN;
            fun_decl->tag.byte_offset = fun->tag.byte_offset;
            fun_decl->ident = fun->ident->name;
            fun_decl->args = args;
            fun_decl->return_type = return_type;

            hash_map_insert(
                &ctx->functions,
                fun_decl->ident,
                strlen(fun_decl->ident),
                fun_decl
            );
        }
    }

    return LOY_OK;
}

static char* format_type_decl(
    Allocator* allocator,
    HashMapEntry* entry,
    u64 indentation
) {
    (void)indentation;
    char* key = (char*)entry->key;
    char* value = format_type(allocator, (Type*)entry->value);

    StringBuilder builder = string_builder_create(allocator);
    string_builder_write_format(&builder, "\"%s\": \"%s\"", key, value);

    return string_builder_to_string(&builder);
}

static char* format_fun_decl(
    Allocator* allocator,
    HashMapEntry* entry,
    u64 indentation
) {
    char* key = (char*)entry->key;
    FunctionDecl* fun = (FunctionDecl*)entry->value;
    indentation++;

    StringBuilder builder = string_builder_create(allocator);
    string_builder_write_format(&builder, "\"%s\": FunDecl{\n", key);

    string_builder_indent(&builder, indentation);
    string_builder_write_format(&builder, ".name = \"%s\"", fun->ident);
    string_builder_write_string(&builder, ",\n");

    string_builder_indent(&builder, indentation);
    char* return_type = format_type(allocator, fun->return_type);
    string_builder_write_format(&builder, ".return_type = \"%s\"", return_type);
    string_builder_write_string(&builder, ",\n");

    string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, ".args = [");
    if(fun->args.len > 0) string_builder_write_string(&builder, "\n");
    indentation++;

    VectorIter args_iter = vector_iter(&fun->args);
    u8 pos = 0;
    while(vector_iter_peek(&args_iter) != NULL) {
        TypedFunArg* arg = (TypedFunArg*)vector_iter_next(&args_iter);

        string_builder_indent(&builder, indentation);
        string_builder_write_format(&builder, "(%u) ", pos);

        char* type = format_type(allocator, arg->type);
        string_builder_write_format(&builder, "%s: ", type);

        string_builder_write_format(&builder, "\"%s\"", arg->ident);
        string_builder_write_string(&builder, ",\n");
        pos++;
    }
    indentation--;

    if(fun->args.len > 0) string_builder_indent(&builder, indentation);
    string_builder_write_string(&builder, "],\n");
    indentation--;

    string_builder_indent(&builder, indentation);
    string_builder_write_byte(&builder, '}');

    return string_builder_to_string(&builder);
}

static bool typer_type_equals(Type* a, Type* b) {
    if(a->kind != b->kind) return false;

    switch(a->kind) {
    case TYPE_FLOAT: {
        TypeFloat* float_a = (TypeFloat*)a;
        TypeFloat* float_b = (TypeFloat*)b;
        return float_a->bit_width == float_b->bit_width;
    }
    case TYPE_USIZE: {
        TypeUsize* usize_a = (TypeUsize*)a;
        TypeUsize* usize_b = (TypeUsize*)b;
        return usize_a->bit_width == usize_b->bit_width;
    }
    case TYPE_ISIZE: {
        TypeIsize* isize_a = (TypeIsize*)a;
        TypeIsize* isize_b = (TypeIsize*)b;
        return isize_a->bit_width == isize_b->bit_width;
    }
    case TYPE_VOID: return a->kind == b->kind;
    case TYPE_INVALID:
    case TYPE_INT:
        break;
    }

    return true;
}

static TypedNode* typer_typecheck_node(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstNode* node,
    Type* expected_type
);

static Type* typer_get_node_type(TypedNode* node) {
    switch(node->tag.kind) {
    case TYPED_NODE_FLOAT_LITERAL: {
        TypedFloat* typed_float = (TypedFloat*)node;
        return (Type*)typed_float->type;
    }
    case TYPED_NODE_INT_LITERAL: {
        TypedInt* typed_int = (TypedInt*)node;
        return (Type*)typed_int->type;
    }
    case TYPED_NODE_UINT_LITERAL: {
        TypedUint* typed_uint = (TypedUint*)node;
        return (Type*)typed_uint->type;
    }
    case TYPED_NODE_FUN_CALL: {
        TypedFunCall* typed_call = (TypedFunCall*)node;
        return typed_call->type;
    }
    case TYPED_NODE_IDENT: {
        TypedIdent* typed_ident = (TypedIdent*)node;
        return typed_ident->type;
    }
    case TYPED_NODE_FUN: {
        TypedFunction* typed_fun = (TypedFunction*)node;
        return typed_fun->return_type;
    }
    case TYPED_NODE_RETURN: {
        TypedReturn* typed_return = (TypedReturn*)node;
        return typed_return->return_type;
    }
    case TYPED_NODE_FUN_ARG:
        break;
    case TYPED_NODE_LET_BINDING:
        break;
    }

    return (Type*)&invalid_type;
}

static TypedLetBinding* typer_typecheck_let_binding(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstLetNode* let
) {
    char* type_name = let->type->ident->name;
    Type* expected_type = typer_assert_type_declaration(
        allocator,
        compile_ctx,
        ctx,
        (AstNode*)let,
        type_name
    );
    if(expected_type == NULL) return NULL;

    TypedNode* value = typer_typecheck_node(allocator, compile_ctx, ctx, let->value, expected_type);
    if(value == NULL) return NULL;

    Type* actual_type = typer_get_node_type(value);

    if(!typer_type_equals(expected_type, actual_type)) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "expected `%s` but got `%s` in let binding",
            type_name,
            format_type(allocator, actual_type)
        );

        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Type error",
            .offset = let->type->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };

        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    typer_scope_add_symbol(ctx, let->ident->name, actual_type);

    TypedLetBinding* let_binding = allocator->alloc(
        allocator->ctx,
        sizeof(TypedLetBinding)
    );
    let_binding->tag.kind = TYPED_NODE_LET_BINDING;
    let_binding->ident = let->ident->name;
    let_binding->type = actual_type;
    let_binding->value = value;
    return let_binding;
}

static TypedFloat* typed_float_create(
    Allocator* allocator,
    f64 value,
    TypeFloat* type
) {
    TypedFloat* typed_float = allocator->alloc(
        allocator->ctx,
        sizeof(TypedFloat)
    );
    typed_float->type = type;
    typed_float->tag.kind = TYPED_NODE_FLOAT_LITERAL;
    typed_float->value = value;
    return typed_float;
}

static TypedInt* typed_int_create(
    Allocator* allocator,
    i64 value,
    TypeInt* type
) {
    TypedInt* typed_int = allocator->alloc(allocator->ctx, sizeof(TypedInt));
    typed_int->type = type;
    typed_int->tag.kind = TYPED_NODE_INT_LITERAL;
    typed_int->value = value;
    return typed_int;
}

static TypedUint* typed_uint_create(
    Allocator* allocator,
    u64 value,
    TypeInt* type
) {
    TypedUint* typed_uint = allocator->alloc(
        allocator->ctx,
        sizeof(TypedUint)
    );
    typed_uint->type = type;
    typed_uint->tag.kind = TYPED_NODE_UINT_LITERAL;
    typed_uint->value = value;
    return typed_uint;
}

static TypedFloat* typer_typecheck_float(
    Allocator* allocator,
    AstFloatNode* float_node,
    Type* expected_type
) {
    if(expected_type == NULL) {
        fprintf(stderr, "Error: float literal must have expected type.\n");
        exit(EXIT_FAILURE);
    }

    if(expected_type->kind != TYPE_FLOAT) {
        fprintf(stderr, "Error: expected float type for float literal.\n");
        exit(EXIT_FAILURE);
    }

    TypeFloat* expected = (TypeFloat*)expected_type;
    const char* value_str = float_node->value;
    double parsed = strtod(value_str, NULL);

    if(expected->bit_width == 32) {
        f32 as_f32 = (f32)parsed;
        f64 widened = (f64)as_f32;

        f64 abs_error = fabs(parsed - widened);
        const f64 epsilon = 1e-6;

        if(abs_error > epsilon) {
            fprintf(
                stderr,
                "Error: float literal %s cannot be represented accurately as f32 (diff = %.10g).\n",
                value_str,
                abs_error
            );
            exit(EXIT_FAILURE);
        }

        return typed_float_create(allocator, as_f32, expected);
    }

    return typed_float_create(allocator, parsed, expected);
}

u64 max_uint_value(u8 bit_width) {
    switch(bit_width) {
    case 8: return UINT8_MAX;
    case 16: return UINT16_MAX;
    case 32: return UINT32_MAX;
    default: return UINT64_MAX;
    }
}

i64 max_int_value(u8 bit_width) {
    switch(bit_width) {
    case 8: return INT8_MAX;
    case 16: return INT16_MAX;
    case 32: return INT32_MAX;
    default: return INT64_MAX;
    }
}

i64 min_int_value(u8 bit_width) {
    switch(bit_width) {
    case 8: return INT8_MIN;
    case 16: return INT16_MIN;
    case 32: return INT32_MIN;
    default: return INT64_MIN;
    }
}

static TypedInt* typer_typecheck_int(
    Allocator* allocator,
    CompileContext* compile_ctx,
    AstIntNode* int_node,
    Type* expected_type
) {
    if(expected_type == NULL) {
        fprintf(stderr, "Error: int literal must have expected type.\n");
        exit(EXIT_FAILURE);
    }

    if(expected_type->kind != TYPE_INT) {
        fprintf(stderr, "Error: expected int type for int literal.\n");
        exit(EXIT_FAILURE);
    }

    TypeInt* expected = (TypeInt*)expected_type;

    // If we expected an unsigned integer but found this signed integer, this is
    // already invalid, so bail early
    if(!expected->is_signed) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "signed literal cannot be assigned to u%u",
            expected->bit_width

        );
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Type error",
            .offset = int_node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    i64 value = int_node->value;
    i64 max = max_int_value(expected->bit_width);
    i64 min = min_int_value(expected->bit_width);

    if(value > (i64)max) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "int literal %ld too large for i%u",
            value,
            expected->bit_width
        );
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_WARN,
            .label = "Invalid value",
            .offset = int_node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
    }

    if(value < min) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "int literal %ld too small for i%u",
            value,
            expected->bit_width
        );
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_WARN,
            .label = "Invalid value",
            .offset = int_node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
    }

    return typed_int_create(allocator, value, expected);
}

static TypedNode* typer_typecheck_uint(
    Allocator* allocator,
    CompileContext* compile_ctx,
    AstUintNode* uint_node,
    Type* expected_type
) {
    if(expected_type == NULL) {
        fprintf(stderr, "Error: uint literal must have expected type.\n");
        exit(EXIT_FAILURE);
    }

    if(expected_type->kind != TYPE_INT) {
        fprintf(stderr, "Error: expected uint type for uint literal.\n");
        exit(EXIT_FAILURE);
    }

    TypeInt* expected = (TypeInt*)expected_type;
    u64 value = uint_node->value;

    if(expected->is_signed) {
        if(value > (u64)max_int_value(expected->bit_width)) {
            StringBuilder builder = string_builder_create(allocator);
            string_builder_write_format(
                &builder,
                "unsigned literal %lu too large for i%u.",
                value,
                expected->bit_width
            );
            Span span = {
                .file = compile_ctx->unit.content,
                .level = SPAN_WARN,
                .label = "Invalid value",
                .offset = uint_node->tag.byte_offset,
                .info = string_builder_to_string(&builder),
            };
            error_report_push_span(&compile_ctx->report, span);
        }

        return (TypedNode*)typed_int_create(allocator, (i64)value, expected);
    }

    if(value > max_uint_value(expected->bit_width)) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "unsigned literal %lu too large for u%u.\n",
            value,
            expected->bit_width
        );
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_WARN,
            .label = "Invalid value",
            .offset = uint_node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
    }

    return (TypedNode*)typed_uint_create(allocator, value, expected);
}

static TypedFunCall* typer_typecheck_fun_call(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstFunCallNode* node,
    Type* expected_type
) {
    char* fun_name = node->ident->name;
    FunctionDecl* signature = hash_map_get(&ctx->functions, fun_name, strlen(fun_name));

    if(signature == NULL) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "calling undeclared function `%s`", fun_name);
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Undeclared function",
            .offset = node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    if(node->args.len != signature->args.len) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "Function expects %d arguments, but got %d.",
            signature->args.len,
            node->args.len
        );
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Not enough arguments",
            .offset = node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    TypedStatements typed_args = vector_create(allocator);
    for(u64 i = 0; i < node->args.len; i++) {
        TypedFunArg* typed = (TypedFunArg*)vector_get(&signature->args, i);
        if(typed == NULL) return NULL;
        AstNode* arg = vector_get(&node->args, i);

        TypedNode* typed_arg = typer_typecheck_node(allocator, compile_ctx, ctx, arg, typed->type);
        if(typed_arg == NULL) return NULL;
        Type* arg_type = typer_get_node_type(typed_arg);

        TypedFunArg* expected_arg = vector_get(&signature->args, i);
        if(expected_arg == NULL) UNREACHABLE();
        Type* expected_arg_type = expected_arg->type;

        if(!typer_type_equals(arg_type, expected_arg_type)) {
            StringBuilder builder = string_builder_create(allocator);
            string_builder_write_format(
                &builder,
                "expected `%s` but got `%s`\n",
                format_type(allocator, arg_type),
                format_type(allocator, expected_arg_type)
            );
            Span span = {
                .file = compile_ctx->unit.content,
                .level = SPAN_ERROR,
                .label = "Type error",
                .offset = arg->byte_offset,
                .info = string_builder_to_string(&builder),
            };
            error_report_push_span(&compile_ctx->report, span);
            return NULL;
        }

        vector_push_ptr(&typed_args, typed_arg);
    }

    if(!typer_type_equals(signature->return_type, expected_type)) {
        char* expected_name = format_type(allocator, expected_type);
        char* actual_name = format_type(allocator, signature->return_type);

        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "expected `%s` but got `%s`\n", expected_name, actual_name);
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Type error",
            .offset = node->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    TypedFunCall* typed_fun_call = allocator->alloc(allocator->ctx, sizeof(TypedFunCall));
    if(typed_fun_call == NULL) return NULL;

    typed_fun_call->tag.kind = TYPED_NODE_FUN_CALL;
    typed_fun_call->ident = node->ident->name;
    typed_fun_call->args = typed_args;
    typed_fun_call->type = expected_type;
    return typed_fun_call;
}

static TypedIdent* typer_typecheck_ident(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstIdentNode* ident,
    Type* expected_type
) {
    char* type_name = ident->name;
    Type* type = typer_scope_get_symbol(ctx, type_name);
    if(type == NULL) type = hash_map_get(&ctx->type_decl, type_name, strlen(type_name));
    if(type == NULL) UNREACHABLE();

    if(expected_type != NULL && !typer_type_equals(type, expected_type)) {
        char* expected_type_name = format_type(allocator, expected_type);
        char* ident_type_name = format_type(allocator, type);
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(&builder, "expected `%s` but got `%s`", expected_type_name, ident_type_name);
        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Type error",
            .offset = ident->tag.byte_offset,
            .info = string_builder_to_string(&builder),
        };
        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    TypedIdent* typed_ident = allocator->alloc(allocator->ctx, sizeof(TypedIdent));
    if(typed_ident == NULL) return NULL;

    typed_ident->tag.kind = TYPED_NODE_IDENT;
    typed_ident->name = ident->name;
    typed_ident->type = type;
    return typed_ident;
}

static TypedReturn* typer_typecheck_return(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstReturnNode* node,
    Type* expected_type
) {
    Type* return_type = (Type*)&type_void;

    TypedReturn* typed_return = allocator->alloc(allocator->ctx, sizeof(TypedReturn));
    if(typed_return == NULL) return NULL;

    typed_return->tag.kind = TYPED_NODE_RETURN;
    typed_return->tag.byte_offset = node->tag.byte_offset;

    if(node->return_value != NULL) {
        TypedNode* typed_node = typer_typecheck_node(allocator, compile_ctx, ctx, node->return_value, expected_type);
        if(typed_node == NULL) return NULL;
        return_type = typer_get_node_type(typed_node);
        typed_return->return_value = typed_node;
    }

    if(!typer_type_equals(return_type, expected_type)) {
        StringBuilder builder = string_builder_create(allocator);
        string_builder_write_format(
            &builder,
            "expected `%s` but got `%s`.",
            format_type(allocator, expected_type),
            format_type(allocator, return_type)
        );

        ByteOffset offset = node->tag.byte_offset;
        if(node->return_value != NULL) offset = node->return_value->byte_offset;

        Span span = {
            .file = compile_ctx->unit.content,
            .level = SPAN_ERROR,
            .label = "Type error",
            .offset = offset,
            .info = string_builder_to_string(&builder),
        };

        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    typed_return->return_type = return_type;
    return typed_return;
}

static TypedFunArg* typer_typecheck_fun_arg(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstFunArgNode* node,
    Type* expected_type
) {
    (void)compile_ctx;
    char* type_name = node->type->ident->name;
    Type* arg_type = hash_map_get(&ctx->type_decl, type_name, strlen(type_name));
    if(!typer_type_equals(arg_type, expected_type)) {
        UNREACHABLE();
    }

    TypedFunArg* arg = allocator->alloc(allocator->ctx, sizeof(TypedFunArg));
    if(arg == NULL) UNREACHABLE();
    arg->tag.kind = TYPED_NODE_FUN_ARG;
    arg->ident = node->ident->name;
    arg->type = arg_type;
    return arg;
}

static TypedNode* typer_typecheck_node(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstNode* node,
    Type* expected_type
) {
    switch(node->kind) {
    case AST_NODE_LET_BINDING: {
        AstLetNode* let_node = (AstLetNode*)node;
        return (TypedNode*)typer_typecheck_let_binding(allocator, compile_ctx, ctx, let_node);
    }
    case AST_NODE_FLOAT_LITERAL: {
        AstFloatNode* float_node = (AstFloatNode*)node;
        return (TypedNode*)typer_typecheck_float(allocator, float_node, expected_type);
    }
    case AST_NODE_INT_LITERAL: {
        AstIntNode* int_node = (AstIntNode*)node;
        return (TypedNode*)typer_typecheck_int(allocator, compile_ctx, int_node, expected_type);
    }
    case AST_NODE_UINT_LITERAL: {
        AstUintNode* uint_node = (AstUintNode*)node;
        return typer_typecheck_uint(allocator, compile_ctx, uint_node, expected_type);
    }
    case AST_NODE_FUN_CALL: {
        AstFunCallNode* fun_call = (AstFunCallNode*)node;
        TypedFunCall* typed_fun_call = typer_typecheck_fun_call(
            allocator,
            compile_ctx,
            ctx,
            fun_call,
            expected_type
        );
        return (TypedNode*)typed_fun_call;
    }
    case AST_NODE_IDENTIFIER: {
        AstIdentNode* ident = (AstIdentNode*)node;
        return (TypedNode*)typer_typecheck_ident(allocator, compile_ctx, ctx, ident, expected_type);
    }
    case AST_NODE_RETURN: {
        AstReturnNode* return_node = (AstReturnNode*)node;
        return (TypedNode*)typer_typecheck_return(allocator, compile_ctx, ctx, return_node, expected_type);
    }
    case AST_NODE_FUN_ARG: {
        AstFunArgNode* arg = (AstFunArgNode*)node;
        return (TypedNode*)typer_typecheck_fun_arg(allocator, compile_ctx, ctx, arg, expected_type);
    }
    case AST_NODE_FUN:
    case AST_NODE_BINARY_OP:
    case AST_NODE_TYPE_ANNOTATION:
        break;
    }

    UNREACHABLE();
}

static TypedFunction* typer_typecheck_function(
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    AstFunNode* fun
) {
    typer_scope_push(allocator, ctx);

    TypedNode* typed_return = typer_typecheck_node(
        allocator,
        compile_ctx,
        ctx,
        (AstNode*)fun->return_type,
        NULL
    );
    if(typed_return == NULL) return NULL;

    Type* return_type = typer_get_node_type(typed_return);
    if(return_type->kind == TYPE_INVALID) {
        Span span = {
            .file = compile_ctx->unit.content,
            .label = "invalid return type of function",
            .level = SPAN_ERROR,
            .offset = fun->return_type->tag.byte_offset,
            .info = "asdasd",
        };
        error_report_push_span(&compile_ctx->report, span);
        return NULL;
    }

    char* fun_name = fun->ident->name;
    FunctionDecl* signature = hash_map_get(&ctx->functions, fun_name, strlen(fun_name));

    Vector typed_args = vector_create(allocator);
    for(u64 i = 0; i < fun->args.len; i++) {
        TypedFunArg* arg_signature = (TypedFunArg*)vector_get(&signature->args, i);
        AstFunArgNode* ast_arg = (AstFunArgNode*)vector_get(&fun->args, i);
        TypedFunArg* typed_arg = (TypedFunArg*)typer_typecheck_node(
            allocator,
            compile_ctx,
            ctx,
            (AstNode*)ast_arg,
            arg_signature->type
        );
        if(typed_arg == NULL) return NULL;
        vector_push_ptr(&typed_args, typed_arg);
    }

    TypedBlock typed_body = {
        .block = vector_create(allocator),
        .return_type = (Type*)&type_void,
    };

    VectorIter body_iter = vector_iter(&fun->body);
    while(vector_iter_peek(&body_iter) != NULL) {
        AstNode* node = (AstNode*)vector_iter_next(&body_iter);

        switch(node->kind) {
        case AST_NODE_LET_BINDING: {
            TypedLetBinding* binding = (TypedLetBinding*)typer_typecheck_node(allocator, compile_ctx, ctx, node, NULL);
            if(binding == NULL) return NULL;
            vector_push_ptr(&typed_body.block, binding);
            break;
        }
        case AST_NODE_RETURN: {
            TypedReturn* typed_node = (TypedReturn*)typer_typecheck_node(
                allocator,
                compile_ctx,
                ctx,
                node,
                return_type
            );
            if(typed_node == NULL) return NULL;
            vector_push_ptr(&typed_body.block, typed_node);
            break;
        }
        case AST_NODE_FUN_CALL: {
            AstFunCallNode* fun_call = (AstFunCallNode*)node;
            TypedFunCall* typed_fun_call = (TypedFunCall*)typer_typecheck_fun_call(
                allocator,
                compile_ctx,
                ctx,
                fun_call,
                NULL
            );
            if(typed_fun_call == NULL) return NULL;
            vector_push_ptr(&typed_body.block, typed_fun_call);
            break;
        }

        case AST_NODE_IDENTIFIER: UNREACHABLE("identifiers are not allowed here");
        case AST_NODE_FUN: UNREACHABLE("function declarations are not allowed here");
        case AST_NODE_FUN_ARG: UNREACHABLE("function args are not allowed here");
        case AST_NODE_BINARY_OP: UNREACHABLE("binary ops are not allowed here");
        case AST_NODE_TYPE_ANNOTATION: UNREACHABLE("type annotations are not allowed here");
        case AST_NODE_INT_LITERAL: UNREACHABLE("int literal is not allowed here");
        case AST_NODE_UINT_LITERAL: UNREACHABLE("uint literal is not allowed here");
        case AST_NODE_FLOAT_LITERAL: UNREACHABLE("float literal is not allowed here");
        }
    }

    // TODO: typecheck that every path returns

    TypedFunction* function = allocator->alloc(allocator->ctx, sizeof(TypedFunction));
    if(function == NULL) return NULL;

    function->tag.kind = TYPED_NODE_FUN;
    function->tag.byte_offset = fun->tag.byte_offset;
    function->ident = fun->ident->name;
    function->args = typed_args;
    function->body = typed_body;
    function->return_type = return_type;

    return function;
}

static LoyResult typer_typecheck_ast_nodes(
    TypedAst* typed_ast,
    Allocator* allocator,
    CompileContext* compile_ctx,
    TyperContext* ctx,
    Ast* ast
) {
    VectorIter nodes_iter = vector_iter(&ast->statements);
    while(vector_iter_peek(&nodes_iter) != NULL) {
        AstNode* node = (AstNode*)vector_iter_next(&nodes_iter);

        switch(node->kind) {
        case AST_NODE_FUN: {
            AstFunNode* function = (AstFunNode*)node;
            TypedFunction* typed = typer_typecheck_function(allocator, compile_ctx, ctx, function);
            if(typed == NULL) return LOY_ERROR_LEXER_INVALID_TOKEN;

            vector_push_ptr(&typed_ast->statements, typed);
            break;
        }
        case AST_NODE_IDENTIFIER:
        case AST_NODE_FUN_ARG:
        case AST_NODE_LET_BINDING:
        case AST_NODE_BINARY_OP:
        case AST_NODE_TYPE_ANNOTATION:
        case AST_NODE_FUN_CALL:
        case AST_NODE_INT_LITERAL:
        case AST_NODE_UINT_LITERAL:
        case AST_NODE_FLOAT_LITERAL: UNREACHABLE();
        case AST_NODE_RETURN:
            break;
        }
    }

    return LOY_OK;
}

LoyResult typer_typecheck_ast(
    CompileContext* compile_ctx,
    TypedAst* typed_ast,
    Allocator* allocator,
    Ast ast
) {
    TyperContext ctx = {
        .type_decl = hash_map_create(allocator),
        .functions = hash_map_create(allocator),
        .scopes = vector_create(allocator),
    };
    Scope root_scope = {
        .symbol_table = hash_map_create(allocator),
        .parent = -1,
    };

    vector_push(&ctx.scopes, root_scope);
    typer_collect_type_decl(&ctx, &ast);

    if(typer_collect_functions(allocator, compile_ctx, &ctx, &ast) != LOY_OK) {
        return LOY_ERROR_PARSER_INVALID_TOKEN;
    }

    if(typer_typecheck_ast_nodes(typed_ast, allocator, compile_ctx, &ctx, &ast) != LOY_OK) {
        return LOY_ERROR_PARSER_INVALID_TOKEN;
    }

    hash_map_inspect(allocator, &ctx.type_decl, format_type_decl);
    hash_map_inspect(allocator, &ctx.functions, format_fun_decl);
    vector_inspect(allocator, &typed_ast->statements, typer_fmt_node);

    return LOY_OK;
}
