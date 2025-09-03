use piller_lexer::{DisplaySource, Span, TokenKind, TokenStream};

use crate::ast::*;
use crate::expr::parse_expr_block;
use crate::result::{ParseIssue, ReportTitle, Result};

#[macro_export]
macro_rules! expect_token {
    ($tokens:expr, $($pat:pat),+ $(,)?) => {{
        let tok = $tokens.next_token();
        if matches!(tok.kind, $($pat)|+) {
            Ok(tok)
        } else {
            super::result::ParseIssue::new(
                concat!("expected one of: ", $(stringify!($pat), " "),+),
                tok.position,
            ).into_error()
        }
    }};
}

pub struct ParseContext<'src> {
    pub tokens: TokenStream,
    pub source: &'src str,
}

pub fn parse_token_stream(ctx: &mut ParseContext<'_>) -> Result<Ast> {
    let mut statements = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::Eof) {
        statements.push(parse_statement(ctx)?);
    }

    Ok(Ast::new(statements))
}

fn parse_statement(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    match ctx.tokens.peek() {
        TokenKind::Type => parse_type_decl(ctx),
        TokenKind::Function => parse_function_decl(ctx),
        _ => todo!(),
    }
}

fn parse_type_decl(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let type_keyword = expect_token!(ctx.tokens, TokenKind::Type)?;
    let type_name = parse_identifier(ctx)?;
    let type_generics = parse_generics_decl(ctx)?;

    // if there is no assign token `=`, theres no way to keep parsing as its ambiguous what
    // would be next.
    expect_token!(ctx.tokens, TokenKind::Assign)?;

    // if there is an invalid type kind after the assign token, there is also no way to keep
    // parsing.
    let (type_kind, ty_position) = expect_token_type_kind(ctx)?;

    expect_token!(ctx.tokens, TokenKind::LBrace)?;

    let value = match type_kind {
        TypeDeclKind::Struct => parse_struct_decl(ctx, ty_position)?,
        TypeDeclKind::Enum => todo!(),
    };

    let close_brace = expect_token!(ctx.tokens, TokenKind::RBrace)?;
    let position = type_keyword.position.merge(close_brace.position);

    Ok(AstNode::TypeDecl(AstNodeTypeDecl {
        name: type_name,
        kind: type_kind,
        value: Box::new(value),
        generics: type_generics,
        position,
    }))
}

fn parse_struct_decl(ctx: &mut ParseContext<'_>, keyword_position: Span) -> Result<AstNode> {
    let mut fields = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let field_name = parse_identifier(ctx)?;
        let field_type = parse_type_annotation(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::RBrace => continue,
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Eof => {
                let prev_token = ctx.tokens.peek_prev_token();
                let position = prev_token.position.merge(field_name.position);
                return ParseIssue::new("unterminated struct declaration", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing brace `}`?")
                    .into_error();
            }
            _ => {
                let prev_token = ctx.tokens.peek_prev_token();
                let position = prev_token.position;
                return ParseIssue::new("struct fields must be separated by a comma", position)
                    .with_report_title("syntax error")
                    .with_help("did you forget to add a comma `,`?")
                    .into_error();
            }
        }

        let position = field_name.position.merge(field_type.position);
        fields.push(AstNodeStructField {
            name: field_name,
            ty: field_type,
            position,
        });
    }

    let position = keyword_position.merge(ctx.tokens.peek_token().position);
    Ok(AstNode::Struct(AstNodeStruct { fields, position }))
}

pub fn parse_type_annotation(ctx: &mut ParseContext<'_>) -> Result<AstNodeTypeAnnotation> {
    expect_token!(ctx.tokens, TokenKind::Colon)?;

    if ctx.tokens.peek().is_primitive() {
        let type_node = parse_primitive_type(ctx);
        return Ok(type_node);
    }

    let type_name = parse_identifier(ctx)?;
    let generics = parse_generics_decl(ctx)?;

    let type_kind = AstNodeTypeKind::Named(AstNodeNamedType {
        name: type_name,
        position: type_name.position,
        generics,
    });

    let type_node = AstNodeTypeAnnotation {
        kind: type_kind,
        position: type_name.position,
    };

    Ok(type_node)
}

fn parse_primitive_type(ctx: &mut ParseContext<'_>) -> AstNodeTypeAnnotation {
    debug_assert!(ctx.tokens.peek().is_primitive());
    let token_primitive = ctx.tokens.next_token();

    let primitive = match token_primitive.kind {
        TokenKind::Bool(value) => PrimitiveTypeKind::Bool(value),
        TokenKind::Integer(bit_size) => PrimitiveTypeKind::Integer(bit_size),
        TokenKind::Unsigned(bit_size) => PrimitiveTypeKind::Unsigned(bit_size),
        _ => unreachable!(),
    };

    AstNodeTypeAnnotation {
        kind: AstNodeTypeKind::Primitive(primitive),
        position: token_primitive.position,
    }
}

fn parse_generics_decl(ctx: &mut ParseContext<'_>) -> Result<Vec<AstNodeGenericDecl>> {
    let mut generics = vec![];

    if !matches!(ctx.tokens.peek(), TokenKind::Lesser) {
        return Ok(generics);
    }

    expect_token!(ctx.tokens, TokenKind::Lesser)?;

    while !matches!(ctx.tokens.peek(), TokenKind::Greater | TokenKind::Eof) {
        let name = parse_identifier(ctx)?;
        let has_inner_generics = matches!(ctx.tokens.peek(), TokenKind::Lesser);
        let inner_generics = if has_inner_generics { parse_generics_decl(ctx)? } else { vec![] };

        let next_token = ctx.tokens.peek_token();
        match next_token.kind {
            TokenKind::Greater => {}
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Eof => {
                let prev_token = ctx.tokens.peek_prev_token();
                let position = prev_token.position.merge(name.position);
                return ParseIssue::new("unterminated generic declaration listing", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing  `>`?")
                    .into_error();
            }
            _ => {
                let prev_token = ctx.tokens.peek_prev_token();
                let position = prev_token.position;
                return ParseIssue::new("generic list must be closed with a `>`", position)
                    .with_report_title("syntax error")
                    .with_help("did you forget to add a closing `>`?")
                    .into_error();
            }
        }

        generics.push(AstNodeGenericDecl {
            name,
            position: name.position,
            generics: inner_generics,
        })
    }

    expect_token!(ctx.tokens, TokenKind::Greater)?;
    Ok(generics)
}

fn parse_function_decl(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let keyword = ctx.tokens.next_token();

    let name = parse_identifier(ctx)?;
    let generics = parse_generics_decl(ctx)?;
    expect_token!(ctx.tokens, TokenKind::LParen)?;
    let args = parse_function_args(ctx)?;
    expect_token!(ctx.tokens, TokenKind::RParen)?;

    let return_ty = if matches!(ctx.tokens.peek(), TokenKind::Colon) {
        Some(parse_type_annotation(ctx)?)
    } else {
        None
    };

    let body = parse_expr_block(ctx)?;

    let position = keyword.position.merge(body.position());
    Ok(AstNode::Function(AstNodeFun {
        name,
        args,
        body,
        generics,
        position,
        return_ty,
    }))
}

fn parse_function_args(ctx: &mut ParseContext<'_>) -> Result<Vec<AstNodeFunArg>> {
    let mut args = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        let name = parse_identifier(ctx)?;
        let ty = parse_type_annotation(ctx)?;
        let position = name.position.merge(ty.position);
        args.push(AstNodeFunArg { name, ty, position })
    }

    Ok(args)
}

fn expect_token_type_kind(ctx: &mut ParseContext<'_>) -> Result<(TypeDeclKind, Span)> {
    let token_type_kind = ctx.tokens.next_token();

    if !matches!(token_type_kind.kind, TokenKind::Struct | TokenKind::Enum) {
        return ParseIssue::new("this is not a valid type kind", token_type_kind.position)
            .with_report_title("unknown type kind")
            .with_help("allowed tokens here could be a `struct` or `enum`, for example")
            .into_error();
    }

    Ok((token_type_kind.kind.into(), token_type_kind.position))
}

pub fn parse_identifier(ctx: &mut ParseContext<'_>) -> Result<AstNodeIdentifier> {
    let ident = ctx.tokens.next_token();

    if matches!(ident.kind, TokenKind::Eof) {
        let position = ctx.tokens.peek_prev_token().position.merge(ident.position);
        return ParseIssue::new("expected identifier for function name", position)
            .with_report_title(ReportTitle::weighted("unexpected end of file (EOF)", 1))
            .with_help("A function must have a valid identifier as its name")
            .into_error();
    }

    if !matches!(ident.kind, TokenKind::Identifier) {
        let identifier_name = ident.display_source(ctx.source);
        let label = format!("value `{identifier_name}` is not a valid identifier");
        return ParseIssue::new(label, ident.position)
            .with_report_title("unexpected token")
            .with_help("identifiers must start with a letter or underscore followed by alphanumeric characters")
            .into_error();
    }

    let position = ident.position;
    Ok(AstNodeIdentifier { position })
}