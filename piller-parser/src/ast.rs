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
pub struct IdentifierExpr {
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TypeDeclKind {
    Struct,
    Enum,
    Interface,
}

impl From<TokenKind> for TypeDeclKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Struct => Self::Struct,
            TokenKind::Enum => Self::Enum,
            TokenKind::Interface => Self::Interface,
            t => unreachable!("{t:?} is not a valid type declaration kind"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct AstNodeTypeDef {
    pub name: IdentifierExpr,
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
pub struct AstNodeEnum {
    pub variants: Vec<AstNodeEnumVariant>,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeEnumVariant {
    pub name: IdentifierExpr,
    pub data: Option<AstNodeTypeAnnotation>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeFunSignature {
    pub name: IdentifierExpr,
    pub generics: Vec<AstNodeGenericDecl>,
    pub args: Vec<AstNodeFunArg>,
    pub return_ty: Option<AstNodeTypeAnnotation>,
    pub position: Span,
}

impl AstNodeFunSignature {
    pub fn into_function_decl(self, body: Expr) -> AstNodeFun {
        AstNodeFun {
            name: self.name,
            generics: self.generics,
            args: self.args,
            return_ty: self.return_ty,
            position: self.position.merge(body.position()),
            body,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeInterface {
    pub functions: Vec<AstNodeFunSignature>,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeStructField {
    pub name: IdentifierExpr,
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
    pub name: IdentifierExpr,
    pub position: Span,
    pub generics: Vec<AstNodeGenericDecl>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeTupleType {
    pub types: Vec<AstNodeTypeAnnotation>,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum AstNodeTypeKind {
    Primitive(PrimitiveTypeKind),
    Named(AstNodeNamedType),
    Tuple(AstNodeTupleType),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeTypeAnnotation {
    pub kind: AstNodeTypeKind,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeGenericDecl {
    pub position: Span,
    pub ty: AstNodeTypeAnnotation,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct AstNodeFun {
    pub name: IdentifierExpr,
    pub generics: Vec<AstNodeGenericDecl>,
    pub return_ty: Option<AstNodeTypeAnnotation>,
    pub args: Vec<AstNodeFunArg>,
    pub position: Span,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeFunArg {
    pub name: IdentifierExpr,
    pub ty: AstNodeTypeAnnotation,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AstNode {
    TypeDef(AstNodeTypeDef),
    Struct(AstNodeStruct),
    Enum(AstNodeEnum),
    Interface(AstNodeInterface),
    Function(AstNodeFun),
}

impl AstNode {
    pub fn position(&self) -> Span {
        match self {
            AstNode::TypeDef(node) => node.position,
            AstNode::Struct(node) => node.position,
            AstNode::Enum(node) => node.position,
            AstNode::Interface(node) => node.position,
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
    pub name: IdentifierExpr,
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
pub struct BoolExpr {
    pub value: bool,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct StringExpr {
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ArrayExpr {
    pub elements: Vec<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct TupleExpr {
    pub elements: Vec<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BinaryExpr {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnaryExpr {
    pub op: Operator,
    pub operand: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct FunctionCallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ArrayAccessExpr {
    pub array: Box<Expr>,
    pub index: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct MemberAccessExpr {
    pub object: Box<Expr>,
    pub member: IdentifierExpr,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub truthy: Box<Expr>,
    pub falsy: Option<Box<Expr>>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct SemiColonExpr {
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct WhileExpr {
    pub position: Span,
    pub condition: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ForExpr {
    pub index_var: Option<IdentifierExpr>,
    pub item_var: IdentifierExpr,
    pub iterable: Box<Expr>,
    pub body: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct StructFieldInitExpr {
    pub field_name: IdentifierExpr,
    pub field_value: Box<Expr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct StructInitExpr {
    pub fields: Vec<StructFieldInitExpr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ReturnExpr {
    pub value: Option<Box<Expr>>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BreakExpr {
    pub value: Option<Box<Expr>>,
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ContinueExpr {
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct CommentExpr {
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct DocCommentExpr {
    pub position: Span,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr {
    Block(BlockExpr),
    Binding(BindingExpr),
    Ident(IdentifierExpr),
    Number(NumberExpr),
    Bool(BoolExpr),
    String(StringExpr),
    Array(ArrayExpr),
    Tuple(TupleExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    FunctionCall(FunctionCallExpr),
    ArrayAccess(ArrayAccessExpr),
    MemberAccess(MemberAccessExpr),
    If(IfExpr),
    SemiColon(SemiColonExpr),
    While(WhileExpr),
    For(ForExpr),
    StructInit(StructInitExpr),
    Return(ReturnExpr),
    Break(BreakExpr),
    Continue(ContinueExpr),
    Comment(CommentExpr),
    DocComment(DocCommentExpr),
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Self::Block(expr) => expr.position,
            Self::Binding(expr) => expr.position,
            Self::Ident(expr) => expr.position,
            Self::Number(expr) => expr.position,
            Self::Bool(expr) => expr.position,
            Self::String(expr) => expr.position,
            Self::Array(expr) => expr.position,
            Self::Tuple(expr) => expr.position,
            Self::Binary(expr) => expr.position,
            Self::Unary(expr) => expr.position,
            Self::FunctionCall(expr) => expr.position,
            Self::ArrayAccess(expr) => expr.position,
            Self::MemberAccess(expr) => expr.position,
            Self::If(expr) => expr.position,
            Self::SemiColon(expr) => expr.position,
            Self::While(expr) => expr.position,
            Self::For(expr) => expr.position,
            Self::StructInit(expr) => expr.position,
            Self::Return(expr) => expr.position,
            Self::Break(expr) => expr.position,
            Self::Continue(expr) => expr.position,
            Self::Comment(expr) => expr.position,
            Self::DocComment(expr) => expr.position,
        }
    }
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
    LBrace,
    Dot,
    Assign,
    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    LShiftAssign,
    RShiftAssign,
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
            TokenKind::LBrace => Operator::LBrace,
            TokenKind::Dot => Operator::Dot,
            TokenKind::Assign => Operator::Assign,
            TokenKind::PlusAssign => Operator::PlusAssign,
            TokenKind::MinusAssign => Operator::MinusAssign,
            TokenKind::MulAssign => Operator::MulAssign,
            TokenKind::DivAssign => Operator::DivAssign,
            TokenKind::ModAssign => Operator::ModAssign,
            TokenKind::BitAndAssign => Operator::BitAndAssign,
            TokenKind::BitOrAssign => Operator::BitOrAssign,
            TokenKind::BitXorAssign => Operator::BitXorAssign,
            TokenKind::LShiftAssign => Operator::LShiftAssign,
            TokenKind::RShiftAssign => Operator::RShiftAssign,
            _ => unreachable!("token kind cannot be converted into an operator"),
        }
    }

    pub fn is_binary(self) -> bool {
        matches!(
            self,
            Operator::Plus
                | Operator::Minus
                | Operator::Star
                | Operator::Div
                | Operator::Mod
                | Operator::Equal
                | Operator::NotEqual
                | Operator::Lesser
                | Operator::Greater
                | Operator::LesserEqual
                | Operator::GreaterEqual
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::BitXor
                | Operator::And
                | Operator::Or
                | Operator::Assign
                | Operator::PlusAssign
                | Operator::MinusAssign
                | Operator::MulAssign
                | Operator::DivAssign
                | Operator::ModAssign
                | Operator::BitAndAssign
                | Operator::BitOrAssign
                | Operator::BitXorAssign
                | Operator::LShiftAssign
                | Operator::RShiftAssign
        )
    }

    pub fn is_right_associative(self) -> bool {
        matches!(
            self,
            Operator::Assign
                | Operator::PlusAssign
                | Operator::MinusAssign
                | Operator::MulAssign
                | Operator::DivAssign
                | Operator::ModAssign
                | Operator::BitAndAssign
                | Operator::BitOrAssign
                | Operator::BitXorAssign
                | Operator::LShiftAssign
                | Operator::RShiftAssign
        )
    }
}