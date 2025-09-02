use piller_lexer::{NumericalBitSize, Span, TokenKind};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
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
    pub name: AstNodeIdentifier,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum AstNode {
    Identifier(AstNodeIdentifier),
    TypeDecl(AstNodeTypeDecl),
    Struct(AstNodeStruct),
}