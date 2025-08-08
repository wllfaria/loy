#include "codegen.h"
#include "defines.h"

static LLVMValueRef generate_node_ir(CodegenContext* ctx, TypedNode* node) {
    switch(node->tag.kind) {
    case TYPED_NODE_INT_LITERAL: {
        TypedInt* int_node = (TypedInt*)node;
        return LLVMConstInt(
            LLVMInt64TypeInContext(ctx->context), // assume all ints are i64 for now
            (u64)int_node->value,
            int_node->type->is_signed
        );
    }

    case TYPED_NODE_RETURN: {
        TypedReturn* ret_node = (TypedReturn*)node;
        LLVMValueRef ret_val = generate_node_ir(ctx, ret_node->return_value);
        return LLVMBuildRet(ctx->builder, ret_val);
    }

    case TYPED_NODE_IDENT: {
        TypedIdent* ident_node = (TypedIdent*)node;
        LLVMValueRef value_ptr = hash_map_get(ctx->variables, ident_node->name, strlen(ident_node->name));
        if(!value_ptr) {
            fprintf(stderr, "Unknown variable: %s\n", ident_node->name);
            exit(1);
        }

        LLVMTypeRef pointee_type = LLVMTypeOf(value_ptr);
        return LLVMBuildLoad2(ctx->builder, pointee_type, value_ptr, "loadtmp");
    }

    case TYPED_NODE_LET_BINDING: {
        TypedLetBinding* let = (TypedLetBinding*)node;
        LLVMValueRef val = generate_node_ir(ctx, let->value);
        if(!val) {
            fprintf(stderr, "Let binding RHS generated NULL IR\n");
            exit(1);
        }

        LLVMValueRef alloca = LLVMBuildAlloca(ctx->builder, LLVMTypeOf(val), let->ident);
        LLVMBuildStore(ctx->builder, val, alloca);
        hash_map_insert(ctx->variables, let->ident, strlen(let->ident), alloca);

        return NULL;
    }

    default:
        UNREACHABLE();
    }
}

static void cogeden_generate_fun_ir(
    Allocator* allocator,
    LLVMModuleRef* module,
    LLVMContextRef* context,
    TypedFunction* fun_node
) {
    LLVMBuilderRef builder = LLVMCreateBuilder();

    LLVMTypeRef return_type = NULL;
    switch(fun_node->return_type->kind) {
    case TYPE_INT: {
        TypeInt* type_int = (TypeInt*)fun_node->return_type;

        if(type_int->bit_width == 8) return_type = LLVMInt8TypeInContext(*context);
        else if(type_int->bit_width == 16) return_type = LLVMInt16TypeInContext(*context);
        else if(type_int->bit_width == 32) return_type = LLVMInt32TypeInContext(*context);
        else if(type_int->bit_width == 64) return_type = LLVMInt64TypeInContext(*context);

        break;
    }
    case TYPE_FLOAT: {
        TypeFloat* type_float = (TypeFloat*)fun_node->return_type;

        if(type_float->bit_width == 32) return_type = LLVMFloatTypeInContext(*context);
        else if(type_float->bit_width == 64) return_type = LLVMDoubleTypeInContext(*context);

        break;
    }
    case TYPE_USIZE:
    case TYPE_ISIZE: {
        return_type = LLVMInt64TypeInContext(*context);
        break;
    }
    case TYPE_VOID: {
        return_type = LLVMVoidTypeInContext(*context);
        break;
    }
    case TYPE_INVALID:
        UNREACHABLE();
    }

    u64 arg_count = fun_node->args.len;
    LLVMTypeRef* param_types = allocator->alloc(allocator->ctx, sizeof(LLVMTypeRef) * arg_count);
    for(u64 i = 0; i < arg_count; ++i) {
        TypedFunArg* arg = (TypedFunArg*)vector_get(&fun_node->args, i);
        switch(arg->type->kind) {
        case TYPE_INT: {
            TypeInt* type_int = (TypeInt*)arg->type;
            if(type_int->bit_width == 8) param_types[i] = LLVMInt8TypeInContext(*context);
            else if(type_int->bit_width == 16) param_types[i] = LLVMInt16TypeInContext(*context);
            else if(type_int->bit_width == 32) param_types[i] = LLVMInt32TypeInContext(*context);
            else if(type_int->bit_width == 64) param_types[i] = LLVMInt64TypeInContext(*context);
            break;
        }
        // Add cases for TYPE_FLOAT, TYPE_USIZE, etc.
        default: UNREACHABLE();
        }
    }

    LLVMTypeRef func_type = LLVMFunctionType(return_type, param_types, (u32)arg_count, false);
    LLVMValueRef function = LLVMAddFunction(*module, fun_node->ident, func_type);

    for(u64 i = 0; i < arg_count; ++i) {
        LLVMValueRef param = LLVMGetParam(function, (u32)i);
        TypedFunArg* arg = (TypedFunArg*)vector_get(&fun_node->args, i);
        LLVMSetValueName(param, arg->ident);
    }

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(*context, function, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);

    HashMap symbol_table = hash_map_create(allocator);
    for(u64 i = 0; i < arg_count; ++i) {
        LLVMValueRef param = LLVMGetParam(function, (u32)i);
        TypedFunArg* arg = (TypedFunArg*)vector_get(&fun_node->args, i);

        LLVMTypeRef arg_type = LLVMTypeOf(param);
        LLVMValueRef alloca = LLVMBuildAlloca(builder, arg_type, arg->ident);
        LLVMBuildStore(builder, param, alloca);
        hash_map_insert(&symbol_table, arg->ident, strlen(arg->ident), alloca);
    }

    CodegenContext cg_ctx = {
        .builder = builder,
        .module = *module,
        .context = *context,
        .function = function,
        .variables = &symbol_table,
    };

    VectorIter body_iter = vector_iter(&fun_node->body.block);
    while(vector_iter_peek(&body_iter) != NULL) {
        TypedNode* stmt = (TypedNode*)vector_iter_next(&body_iter);
        generate_node_ir(&cg_ctx, stmt);
    }

    LLVMDisposeBuilder(builder);
    hash_map_destroy(&symbol_table);
}

void cogeden_generate_llvm_ir(
    Allocator* allocator,
    LLVMModuleRef module,
    LLVMContextRef context,
    TypedAst* ast
) {
    VectorIter iter = vector_iter(&ast->statements);

    while(vector_iter_peek(&iter) != NULL) {
        TypedNode* node = (TypedNode*)vector_iter_next(&iter);

        switch(node->tag.kind) {
        case TYPED_NODE_FUN: {
            TypedFunction* fun_node = (TypedFunction*)node;
            cogeden_generate_fun_ir(allocator, &module, &context, fun_node);
            break;
        }
        case TYPED_NODE_FUN_ARG:
        case TYPED_NODE_FLOAT_LITERAL:
        case TYPED_NODE_INT_LITERAL:
        case TYPED_NODE_UINT_LITERAL:
        case TYPED_NODE_LET_BINDING:
        case TYPED_NODE_IDENT:
        case TYPED_NODE_FUN_CALL:
        case TYPED_NODE_RETURN:
            break;
        }
    }

    LLVMPrintModuleToFile(module, "output.ll", NULL);
}
