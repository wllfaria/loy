#ifndef _CODEGEN_H
#define _CODEGEN_H

#include <llvm-c/Core.h>

#include "typer.h"

typedef struct {
    LLVMBuilderRef builder;
    LLVMModuleRef  module;
    LLVMContextRef context;
    LLVMValueRef   function;
    HashMap*       variables; // char* -> LLVMValueRef (allocas)
} CodegenContext;

void cogeden_generate_llvm_ir(
    Allocator* allocator,
    LLVMModuleRef module,
    LLVMContextRef context,
    TypedAst* ast
);

#endif
