#![allow(dead_code)]
use piller_lexer::{NumericalBitSize, Span, TokenKind};

pub trait AstFmt {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result;
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Ast {
    statements: Vec<AstNode>,
}

struct AstDisplay<'a> {
    ast: &'a Ast,
    source: &'a str,
}

impl<'a> std::fmt::Display for AstDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<Root>")?;

        for (idx, statement) in self.ast.statements.iter().enumerate() {
            let is_last = idx == self.ast.statements.len() - 1;
            statement.fmt_ast(f, self.source, "", is_last)?;
        }

        Ok(())
    }
}

impl Ast {
    pub(crate) fn new(statements: Vec<AstNode>) -> Self {
        Self { statements }
    }

    pub fn dump(&self, source: &str) -> String {
        let display = AstDisplay { ast: self, source };
        format!("{display}")
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

impl std::fmt::Display for TypeDeclKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct => write!(f, "struct"),
            Self::Enum => write!(f, "enum"),
            Self::Interface => write!(f, "interface"),
        }
    }
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
    pub visibility: NodeVisibility,
    pub name: IdentifierExpr,
    pub kind: TypeDeclKind,
    pub value: Box<AstNode>,
    pub generics: Vec<AstNodeGenericDecl>,
    pub position: Span,
}

impl AstFmt for AstNodeTypeDef {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        let name = &source[self.name.position.into_range()];
        writeln!(f, "Typedef {} <{}>", self.kind, name)?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if !self.generics.is_empty() {
            writeln!(f, "{child_prefix}├─ with generics:")?;

            for (idx, generic) in self.generics.iter().enumerate() {
                let is_last = idx == self.generics.len() - 1;
                let child_prefix = format!("{child_prefix}│  ");
                generic.fmt_ast(f, source, &child_prefix, is_last)?;
            }
        }

        let kind = match self.kind {
            TypeDeclKind::Enum => "variants",
            TypeDeclKind::Struct => "fields",
            TypeDeclKind::Interface => "contract",
        };
        writeln!(f, "{child_prefix}└─ with {kind}:")?;
        let child_prefix = format!("{child_prefix}   ");
        self.value.fmt_ast(f, source, &child_prefix, true)?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeStruct {
    pub visibility: NodeVisibility,
    pub fields: Vec<AstNodeStructField>,
    pub position: Span,
}

impl AstFmt for AstNodeStruct {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        _: bool,
    ) -> std::fmt::Result {
        for (idx, field) in self.fields.iter().enumerate() {
            let is_last = idx == self.fields.len() - 1;
            field.fmt_ast(f, source, prefix, is_last)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeEnum {
    pub visibility: NodeVisibility,
    pub variants: Vec<AstNodeEnumVariant>,
    pub position: Span,
}

impl AstFmt for AstNodeEnum {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        _: bool,
    ) -> std::fmt::Result {
        for (idx, variant) in self.variants.iter().enumerate() {
            let is_last = idx == self.variants.len() - 1;
            variant.fmt_ast(f, source, prefix, is_last)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeEnumVariant {
    pub name: IdentifierExpr,
    pub data: Option<AstNodeTypeAnnotation>,
}

impl AstFmt for AstNodeEnumVariant {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}<{}>",
            &source[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        if let Some(data) = self.data.as_ref() {
            data.fmt_ast(f, source, &child_prefix, is_last)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeFunSignature {
    pub name: IdentifierExpr,
    pub generics: Vec<AstNodeGenericDecl>,
    pub args: Vec<AstNodeFunArg>,
    pub return_ty: Option<AstNodeTypeAnnotation>,
    pub position: Span,
}

impl AstFmt for AstNodeFunSignature {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}Function <{}>",
            &source[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        writeln!(f, "{child_prefix}├─ returning:")?;

        match self.return_ty.as_ref() {
            Some(return_ty) => {
                let child_prefix = format!("{child_prefix}│  ");
                return_ty.fmt_ast(f, source, &child_prefix, true)?;
            }
            None => write!(f, "void")?,
        };

        if !self.generics.is_empty() {
            writeln!(f, "{child_prefix}├─ with generics:")?;
            for (idx, generic) in self.generics.iter().enumerate() {
                let is_last = idx == self.generics.len() - 1;
                let child_prefix = format!("{child_prefix}│  ");
                generic.fmt_ast(f, source, &child_prefix, is_last)?;
            }
        }

        if !self.args.is_empty() {
            writeln!(f, "{child_prefix}└─ with args:")?;
            for (idx, arg) in self.args.iter().enumerate() {
                let is_last = idx == self.args.len() - 1;
                let child_branch = if is_last { "   " } else { "│  " };
                let child_prefix = format!("{child_prefix}{child_branch}");
                arg.fmt_ast(f, source, &child_prefix, is_last)?;
            }
        }

        Ok(())
    }
}

impl AstNodeFunSignature {
    pub fn into_function_decl(self, body: Expr) -> AstNodeFun {
        AstNodeFun {
            visibility: NodeVisibility::Private,
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
    pub visibility: NodeVisibility,
    pub functions: Vec<AstNodeFunSignature>,
    pub position: Span,
}

impl AstFmt for AstNodeInterface {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        for (idx, function) in self.functions.iter().enumerate() {
            let is_last = idx == self.functions.len() - 1;
            function.fmt_ast(f, source, prefix, is_last)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeStructField {
    pub name: IdentifierExpr,
    pub ty: AstNodeTypeAnnotation,
    pub position: Span,
}

impl AstFmt for AstNodeStructField {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}<{}> with type:",
            &source[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        self.ty.fmt_ast(f, source, &child_prefix, true)?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PrimitiveTypeKind {
    Bool(bool),
    Unsigned(NumericalBitSize),
    Integer(NumericalBitSize),
}

impl AstFmt for PrimitiveTypeKind {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        write!(f, "primitive")
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeNamedType {
    pub name: IdentifierExpr,
    pub position: Span,
    pub generics: Vec<AstNodeGenericDecl>,
}

impl AstFmt for AstNodeNamedType {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        write!(f, "<{}>", &source[self.name.position.into_range()])?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if !self.generics.is_empty() {
            writeln!(f, " with generics:")?;

            for (idx, generic) in self.generics.iter().enumerate() {
                let is_last = idx == self.generics.len() - 1;
                generic.fmt_ast(f, source, &child_prefix, is_last)?;
            }
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeTupleType {
    pub types: Vec<AstNodeTypeAnnotation>,
    pub position: Span,
}

impl AstFmt for AstNodeTupleType {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        _: bool,
    ) -> std::fmt::Result {
        if !self.types.is_empty() {
            for (idx, ty) in self.types.iter().enumerate() {
                let is_last = idx == self.types.len() - 1;
                ty.fmt_ast(f, source, prefix, is_last)?;
            }
        }

        Ok(())
    }
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

impl AstFmt for AstNodeTypeAnnotation {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        match &self.kind {
            AstNodeTypeKind::Primitive(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNodeTypeKind::Named(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNodeTypeKind::Tuple(node) => node.fmt_ast(f, source, prefix, is_last),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeGenericDecl {
    pub position: Span,
    pub ty: AstNodeTypeAnnotation,
}

impl AstFmt for AstNodeGenericDecl {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        self.ty.fmt_ast(f, source, prefix, is_last)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct AstNodeFun {
    pub name: IdentifierExpr,
    pub generics: Vec<AstNodeGenericDecl>,
    pub args: Vec<AstNodeFunArg>,
    pub position: Span,
    pub visibility: NodeVisibility,
    pub return_ty: Option<AstNodeTypeAnnotation>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeFunArg {
    pub name: IdentifierExpr,
    pub ty: AstNodeTypeAnnotation,
    pub position: Span,
}

impl AstFmt for AstNodeFunArg {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}<{}> with type:",
            &source[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        self.ty.fmt_ast(f, source, &child_prefix, true)?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeImportPath {
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeImportAlias {
    pub position: Span,
    pub name: IdentifierExpr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeImportMethods {
    pub methods: Vec<IdentifierExpr>,
    pub position: Span,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct AstNodeImport {
    pub path: AstNodeImportPath,
    pub alias: Option<AstNodeImportAlias>,
    pub methods: Option<AstNodeImportMethods>,
    pub position: Span,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum NodeVisibility {
    #[default]
    Private,
    Public,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AstNode {
    TypeDef(AstNodeTypeDef),
    Struct(AstNodeStruct),
    Enum(AstNodeEnum),
    Interface(AstNodeInterface),
    Function(Box<AstNodeFun>),
    Import(AstNodeImport),
}

impl AstNode {
    pub fn position(&self) -> Span {
        match self {
            AstNode::TypeDef(node) => node.position,
            AstNode::Struct(node) => node.position,
            AstNode::Enum(node) => node.position,
            AstNode::Interface(node) => node.position,
            AstNode::Function(node) => node.position,
            AstNode::Import(node) => node.position,
        }
    }

    pub fn set_visibility(&mut self, visibility: NodeVisibility) {
        match self {
            AstNode::TypeDef(node) => node.visibility = visibility,
            AstNode::Struct(node) => node.visibility = visibility,
            AstNode::Enum(node) => node.visibility = visibility,
            AstNode::Interface(node) => node.visibility = visibility,
            AstNode::Function(node) => node.visibility = visibility,
            // imports have no visibility modifiers
            AstNode::Import(_) => {}
        }
    }
}

impl AstFmt for AstNode {
    fn fmt_ast(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: &str,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        match self {
            AstNode::TypeDef(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Struct(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Enum(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Interface(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Function(node) => todo!(),
            AstNode::Import(node) => todo!(),
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
