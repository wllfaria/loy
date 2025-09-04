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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Operator {
    Minus,
    Star,
    Div,
    Plus,
    Mod,
    Increment,
    Decrement,
    Equal,
    NotEqual,
    Lesser,
    Greater,
    LesserEqual,
    GreaterEqual,
    Not,
    Or,
    And,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    LParen,
    LBracket,
    Dot,
}

impl Operator {
    pub fn from_token_kind(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Minus => Operator::Minus,
            TokenKind::Star => Operator::Star,
            TokenKind::Div => Operator::Div,
            TokenKind::Plus => Operator::Plus,
            TokenKind::Mod => Operator::Mod,
            TokenKind::Increment => Operator::Increment,
            TokenKind::Decrement => Operator::Decrement,
            TokenKind::Equal => Operator::Equal,
            TokenKind::NotEqual => Operator::NotEqual,
            TokenKind::Lesser => Operator::Lesser,
            TokenKind::Greater => Operator::Greater,
            TokenKind::LesserEqual => Operator::LesserEqual,
            TokenKind::GreaterEqual => Operator::GreaterEqual,
            TokenKind::Not => Operator::Not,
            TokenKind::Or => Operator::Or,
            TokenKind::And => Operator::And,
            TokenKind::BitNot => Operator::BitNot,
            TokenKind::BitAnd => Operator::BitAnd,
            TokenKind::BitOr => Operator::BitOr,
            TokenKind::BitXor => Operator::BitXor,
            TokenKind::LParen => Operator::LParen,
            TokenKind::LBracket => Operator::LBracket,
            TokenKind::Dot => Operator::Dot,
            _ => unreachable!("token kind cannot be converted into an operator"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BinaryExpr {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr {
    Block(BlockExpr),
    Binding(BindingExpr),
    Number(NumberExpr),
    Binary(BinaryExpr),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Expr::Block(expr) => expr.position,
            Expr::Binding(expr) => expr.position,
            Expr::Number(expr) => expr.position,
            Expr::Binary(expr) => expr.position,
        }
    }
}
