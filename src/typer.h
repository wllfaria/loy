#ifndef _TYPER_H
#define _TYPER_H

#include "collections/hash_map.h"
#include "mem/allocator.h"
#include "parser.h"

typedef Vector TypedStatements;

typedef enum {
    TYPE_INVALID = 0,
    TYPE_FLOAT,
    TYPE_USIZE,
    TYPE_ISIZE,
    TYPE_INT,
} TypeKind;

typedef struct {
    TypeKind kind;
} Type;

typedef struct {
    TypeKind kind;
} TypeInvalid;

typedef struct {
    TypeKind kind;
    u8       bit_width; // 32 or 64
} TypeUsize;

typedef struct {
    TypeKind kind;
    u8       bit_width; // 32 or 64
} TypeIsize;

typedef struct {
    TypeKind kind;
    u8       bit_width; // 32 or 64
} TypeFloat;

typedef struct {
    TypeKind kind;
    u8       bit_width; // 8, 16, 32 or 64
    bool     is_signed;
} TypeInt;

typedef struct {
    const char* name;
    Type*       type;
} PrimitiveTypeEntry;

typedef enum {
    TYPED_NODE_FUN,
    TYPED_NODE_FUN_ARG,
    TYPED_NODE_FLOAT_LITERAL,
    TYPED_NODE_INT_LITERAL,
    TYPED_NODE_UINT_LITERAL,
    TYPED_NODE_LET_BINDING,
    TYPED_NODE_IDENT,
    TYPED_NODE_FUN_CALL,
} TypedNodeKind;

typedef struct {
    TypedNodeKind kind;
} TypedNodeTag;

typedef struct {
    TypedNodeTag tag;
} TypedNode;

typedef struct {
    TypedNodeTag tag;
    Type*        type;
} TypedIdent;

typedef struct {
    TypedNodeTag tag;
    TypeFloat*   type;
    f64          value;
} TypedFloat;

typedef struct {
    TypedNodeTag tag;
    TypeInt*     type;
    i64          value;
} TypedInt;

typedef struct {
    TypedNodeTag tag;
    TypeInt*     type;
    u64          value;
} TypedUint;

typedef struct {
    TypedNodeTag tag;
    char*        ident;
    Type*        type;
    TypedNode*   value;
} TypedLetBinding;

typedef struct {
    TypedNodeTag    tag;
    char*           ident;
    TypedStatements args;
    Type*           type;
} TypedFunCall;

typedef struct {
    TypedNodeTag tag;
    char*        ident;
    Type*        type;
} TypedFunArg;

typedef struct {
    TypedNodeTag tag;
    char*        ident;
    Vector       args;
    Type*        return_type;
} FunctionDecl;

typedef struct {
    TypedNodeTag    tag;
    char*           ident;
    TypedStatements args;
    TypedStatements body;
    Type*           return_type;
} TypedFunction;

typedef struct {
    TypedStatements statements;
} TypedAst;

typedef struct {
    HashMap type_decl;
    HashMap functions;
    Vector  scopes;
} TyperContext;

typedef struct {
    HashMap symbol_table;
    i64     parent;
} Scope;

// static void typer_typecheck_ast_node(AstNode* node);
// static void typer_typecheck_function(AstFunNode* node);

void typer_typecheck_ast(Allocator* allocator, Ast ast);

#endif
