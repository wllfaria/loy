#include "codegen.h"
#include "defines.h"
#include <llvm-c/Core.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/DataTypes.h>

static TypeEntry* generate_node_ir(
    Allocator* allocator,
    CodegenFnContext* ctx,
    CodegenContext* codegen_ctx,
    TypedNode* node
) {
    switch(node->tag.kind) {
    case TYPED_NODE_INT_LITERAL: {
        TypedInt* int_node = (TypedInt*)node;

        LLVMTypeRef type_ref = LLVMInt64TypeInContext(ctx->context);
        LLVMValueRef value_ref = LLVMConstInt(type_ref, (u64)int_node->value, int_node->type->is_signed);

        TypeEntry* entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
        entry->type_ref = type_ref;
        entry->value_ref = value_ref;
        return entry;
    }

    case TYPED_NODE_RETURN: {
        TypedReturn* ret_node = (TypedReturn*)node;

        TypeEntry* entry;
        if(ret_node->return_value != NULL) {
            entry = generate_node_ir(allocator, ctx, codegen_ctx, ret_node->return_value);
            LLVMValueRef value_ref = LLVMBuildRet(ctx->builder, entry->value_ref);

            TypeEntry* new_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
            new_entry->type_ref = entry->type_ref;
            new_entry->value_ref = value_ref;
            return new_entry;
        } else {
            LLVMBuildRetVoid(ctx->builder);

            TypeEntry* new_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
            new_entry->type_ref = LLVMVoidType();
            new_entry->value_ref = NULL;
            return new_entry;
        }
    }

    case TYPED_NODE_IDENT: {
        TypedIdent* ident_node = (TypedIdent*)node;
        TypeEntry* entry = hash_map_get(ctx->variables, ident_node->name, strlen(ident_node->name));
        if(!entry->value_ref) exit(1);

        LLVMValueRef value_ref = LLVMBuildLoad2(ctx->builder, entry->type_ref, entry->value_ref, "loadtmp");
        TypeEntry* new_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
        new_entry->value_ref = value_ref;
        new_entry->type_ref = entry->type_ref;
        return new_entry;
    }

    case TYPED_NODE_LET_BINDING: {
        TypedLetBinding* let = (TypedLetBinding*)node;
        TypeEntry* entry = generate_node_ir(allocator, ctx, codegen_ctx, let->value);
        if(!entry->value_ref) exit(1);

        LLVMValueRef alloca = LLVMBuildAlloca(ctx->builder, entry->type_ref, let->ident);

        TypeEntry* new_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
        new_entry->value_ref = alloca;
        new_entry->type_ref = entry->type_ref;

        LLVMBuildStore(ctx->builder, entry->value_ref, alloca);

        hash_map_insert(ctx->variables, let->ident, strlen(let->ident), new_entry);
        return entry;
    }

    case TYPED_NODE_BINARY_OP: {
        TypedBinaryOp* binop = (TypedBinaryOp*)node;

        TypeEntry* lhs_entry = generate_node_ir(allocator, ctx, codegen_ctx, binop->lhs);
        if(!lhs_entry->value_ref) exit(1);

        TypeEntry* rhs_entry = generate_node_ir(allocator, ctx, codegen_ctx, binop->rhs);
        if(!rhs_entry->value_ref) exit(1);

        LLVMValueRef result_ref = NULL;
        switch(binop->op) {
        case TOKEN_PLUS: {
            LLVMTypeRef lhs_type = lhs_entry->type_ref;
            if(LLVMGetTypeKind(lhs_type) == LLVMIntegerTypeKind) {
                result_ref = LLVMBuildAdd(ctx->builder, lhs_entry->value_ref, rhs_entry->value_ref, "addtmp");
            } else if(LLVMGetTypeKind(lhs_type) == LLVMFloatTypeKind ||
                      LLVMGetTypeKind(lhs_type) == LLVMDoubleTypeKind) {
                result_ref = LLVMBuildFAdd(ctx->builder, lhs_entry->value_ref, rhs_entry->value_ref, "faddtmp");
            } else {
                fprintf(stderr, "Unsupported type for addition\n");
                exit(1);
            }

            break;
        }

        case TOKEN_EQUAL:
        case TOKEN_COLON:
        case TOKEN_ASSIGN_ADD:
        case TOKEN_IDENT:
        case TOKEN_LET:
        case TOKEN_COMMA:
        case TOKEN_FUN:
        case TOKEN_RETURN:
        case TOKEN_INT:
        case TOKEN_FLOAT:
        case TOKEN_SEMI:
        case TOKEN_LPAREN:
        case TOKEN_RPAREN:
        case TOKEN_LBRACE:
        case TOKEN_RBRACE:
        case TOKEN_THIN_ARROW:
        case TOKEN_PLUS_PLUS:
            UNREACHABLE();
        }

        TypeEntry* new_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
        new_entry->type_ref = lhs_entry->type_ref;
        new_entry->value_ref = result_ref;
        return new_entry;
    }

    case TYPED_NODE_FUN_CALL: {
        TypedFunCall* call = (TypedFunCall*)node;

        u64 arg_count = call->args.len;
        LLVMValueRef* arg_values = allocator->alloc(allocator->ctx, sizeof(LLVMValueRef) * arg_count);

        for(u64 i = 0; i < arg_count; i++) {
            TypedNode* arg_node = (TypedNode*)vector_get(&call->args, i);
            TypeEntry* arg_entry = generate_node_ir(allocator, ctx, codegen_ctx, arg_node);
            if(!arg_entry || !arg_entry->value_ref) exit(1);
            arg_values[i] = arg_entry->value_ref;
        }

        TypeEntry* fun_entry = hash_map_get(&codegen_ctx->functions, call->ident, strlen(call->ident));

        LLVMValueRef call_result = LLVMBuildCall2(
            ctx->builder,
            fun_entry->type_ref,
            fun_entry->value_ref,
            arg_values,
            (unsigned)arg_count,
            "calltmp"
        );

        LLVMTypeRef ret_type = LLVMGetReturnType(fun_entry->type_ref);
        TypeEntry* entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
        entry->type_ref = ret_type;
        entry->value_ref = LLVMGetTypeKind(ret_type) == LLVMVoidTypeKind ? NULL : call_result;

        return entry;
    }

    default:
        printf("%d\n\n\n", node->tag.kind);
        UNREACHABLE();
    }
}

static void cogeden_generate_fun_ir(
    Allocator* allocator,
    LLVMModuleRef module,
    LLVMContextRef context,
    CodegenContext* ctx,
    TypedFunction* fun_node
) {
    LLVMBuilderRef builder = LLVMCreateBuilder();

    LLVMTypeRef return_type = NULL;
    switch(fun_node->return_type->kind) {
    case TYPE_INT: {
        TypeInt* type_int = (TypeInt*)fun_node->return_type;

        if(type_int->bit_width == 8) return_type = LLVMInt8TypeInContext(context);
        else if(type_int->bit_width == 16) return_type = LLVMInt16TypeInContext(context);
        else if(type_int->bit_width == 32) return_type = LLVMInt32TypeInContext(context);
        else if(type_int->bit_width == 64) return_type = LLVMInt64TypeInContext(context);

        break;
    }
    case TYPE_FLOAT: {
        TypeFloat* type_float = (TypeFloat*)fun_node->return_type;

        if(type_float->bit_width == 32) return_type = LLVMFloatTypeInContext(context);
        else if(type_float->bit_width == 64) return_type = LLVMDoubleTypeInContext(context);

        break;
    }
    case TYPE_USIZE:
    case TYPE_ISIZE: {
        return_type = LLVMInt64TypeInContext(context);
        break;
    }
    case TYPE_VOID: {
        return_type = LLVMVoidTypeInContext(context);
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
            if(type_int->bit_width == 8) param_types[i] = LLVMInt8TypeInContext(context);
            else if(type_int->bit_width == 16) param_types[i] = LLVMInt16TypeInContext(context);
            else if(type_int->bit_width == 32) param_types[i] = LLVMInt32TypeInContext(context);
            else if(type_int->bit_width == 64) param_types[i] = LLVMInt64TypeInContext(context);
            break;
        }
        // Add cases for TYPE_FLOAT, TYPE_USIZE, etc.
        default: UNREACHABLE();
        }
    }

    LLVMTypeRef func_type = LLVMFunctionType(return_type, param_types, (u32)arg_count, false);
    LLVMValueRef function = LLVMAddFunction(module, fun_node->ident, func_type);

    TypeEntry* type_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
    type_entry->type_ref = func_type;
    type_entry->value_ref = function;
    hash_map_insert(&ctx->functions, fun_node->ident, strlen(fun_node->ident), type_entry);

    for(u64 i = 0; i < arg_count; ++i) {
        LLVMValueRef param = LLVMGetParam(function, (u32)i);
        TypedFunArg* arg = (TypedFunArg*)vector_get(&fun_node->args, i);
        LLVMSetValueName(param, arg->ident);
    }

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, function, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);

    HashMap symbol_table = hash_map_create(allocator);
    for(u64 i = 0; i < arg_count; ++i) {
        LLVMValueRef param = LLVMGetParam(function, (u32)i);
        TypedFunArg* arg = (TypedFunArg*)vector_get(&fun_node->args, i);

        LLVMTypeRef arg_type = LLVMTypeOf(param);
        LLVMValueRef alloca = LLVMBuildAlloca(builder, arg_type, arg->ident);
        LLVMBuildStore(builder, param, alloca);
        TypeEntry* arg_entry = allocator->alloc(allocator->ctx, sizeof(TypeEntry));
        arg_entry->type_ref = arg_type;
        arg_entry->value_ref = alloca;

        hash_map_insert(&symbol_table, arg->ident, strlen(arg->ident), arg_entry);
    }

    CodegenFnContext cg_ctx = {
        .builder = builder,
        .module = module,
        .context = context,
        .function = function,
        .variables = &symbol_table,
    };

    VectorIter body_iter = vector_iter(&fun_node->body.block);
    while(vector_iter_peek(&body_iter) != NULL) {
        TypedNode* stmt = (TypedNode*)vector_iter_next(&body_iter);
        generate_node_ir(allocator, &cg_ctx, ctx, stmt);
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
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmPrinters();
    LLVMInitializeAllAsmParsers();

    LLVMTargetRef target;
    char* error = NULL;

    if(LLVMGetTargetFromTriple(LLVMGetDefaultTargetTriple(), &target, &error)) {
        fprintf(stderr, "Error: %s\n", error);
        LLVMDisposeMessage(error);
        exit(1);
    }

    LLVMTargetMachineRef tm = LLVMCreateTargetMachine(
        target,
        LLVMGetDefaultTargetTriple(),
        "",
        "",
        LLVMCodeGenLevelDefault,
        LLVMRelocDefault,
        LLVMCodeModelDefault
    );
    LLVMSetModuleDataLayout(module, LLVMCreateTargetDataLayout(tm));

    CodegenContext ctx = { .functions = hash_map_create(allocator), };

    VectorIter iter = vector_iter(&ast->statements);
    while(vector_iter_peek(&iter) != NULL) {
        TypedNode* node = (TypedNode*)vector_iter_next(&iter);

        switch(node->tag.kind) {
        case TYPED_NODE_FUN: {
            TypedFunction* fun_node = (TypedFunction*)node;
            cogeden_generate_fun_ir(allocator, module, context, &ctx, fun_node);
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
        case TYPED_NODE_BINARY_OP:
            break;
        }
    }

    char* obj_filename = "output.o";
    if(LLVMTargetMachineEmitToFile(tm, module, obj_filename, LLVMObjectFile, &error) != 0) {
        fprintf(stderr, "Failed to emit object file: %s\n", error);
        LLVMDisposeMessage(error);
        exit(1);
    }

    int ret = system("cc output.o -o output");
    if(ret != 0) {
        fprintf(stderr, "Linking failed\n");
        exit(1);
    }
}
