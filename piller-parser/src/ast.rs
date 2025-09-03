#![allow(dead_code)]
use piller_lexer::{NumericalBitSize, Span, TokenKind};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Ast {
    statements: Vec<AstNode>,
}

impl Ast {
    pub(crate) fn new(statements: Vec<AstNode>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct AstNodeIdentifier {
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TypeDeclKind {
    Struct,
    Enum,
}

impl From<TokenKind> for TypeDeclKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Struct => Self::Struct,
            TokenKind::Enum => Self::Enum,
            t => unreachable!("{t:?} is not a valid type declaration kind"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct AstNodeTypeDecl {
    pub name: AstNodeIdentifier,
    pub kind: TypeDeclKind,
    pub value: Box<AstNode>,
    pub generics: Vec<AstNodeGenericDecl>,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeStruct {
    pub fields: Vec<AstNodeStructField>,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeStructField {
    pub name: AstNodeIdentifier,
    pub ty: AstNodeTypeAnnotation,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PrimitiveTypeKind {
    Bool(bool),
    Unsigned(NumericalBitSize),
    Integer(NumericalBitSize),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeNamedType {
    pub name: AstNodeIdentifier,
    pub position: Span,
    pub generics: Vec<AstNodeGenericDecl>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum AstNodeTypeKind {
    Primitive(PrimitiveTypeKind),
    Named(AstNodeNamedType),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeTypeAnnotation {
    pub kind: AstNodeTypeKind,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeGenericDecl {
    pub position: Span,
    pub name: AstNodeIdentifier,
    pub generics: Vec<AstNodeGenericDecl>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct AstNodeFun {
    pub name: AstNodeIdentifier,
    pub generics: Vec<AstNodeGenericDecl>,
    pub return_ty: Option<AstNodeTypeAnnotation>,
    pub args: Vec<AstNodeFunArg>,
    pub position: Span,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeFunArg {
    pub name: AstNodeIdentifier,
    pub ty: AstNodeTypeAnnotation,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AstNode {
    TypeDecl(AstNodeTypeDecl),
    Struct(AstNodeStruct),
    Function(AstNodeFun),
}

impl AstNode {
    pub fn position(&self) -> Span {
        match self {
            AstNode::TypeDecl(node) => node.position,
            AstNode::Struct(node) => node.position,
            AstNode::Function(node) => node.position,
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BlockExpr {
    pub position: Span,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum BindingMutability {
    Immutable,
    Mutable,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BindingExpr {
    pub mutability: BindingMutability,
    pub name: AstNodeIdentifier,
    pub ty: Option<AstNodeTypeAnnotation>,
    pub value: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum NumberKindExpr {
    Unsigned(u64),
    Signed(i64),
    Float(f64),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct NumberExpr {
    pub kind: NumberKindExpr,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr {
    Block(BlockExpr),
    Binding(BindingExpr),
    Number(NumberExpr),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Expr::Block(expr) => expr.position,
            Expr::Binding(expr) => expr.position,
            Expr::Number(expr) => expr.position,
        }
    }
}