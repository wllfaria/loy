use piller_lexer::{Span, TokenKind};

use super::functions::parse_function_signature;
use crate::ast::*;
use crate::expr::parse_identifier;
use crate::result::ParseIssue;
use crate::statement::{parse_generics_list, parse_tuple_type, parse_type_annotation};
use crate::{ParseContext, Result};

pub fn parse_type_definition(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let type_keyword = ctx.tokens.next_token();
    if !matches!(type_keyword.kind, TokenKind::Type) {
        let message = format!("expected keyword `type`, but found {}", type_keyword.kind);
        return ParseIssue::new(message, type_keyword.position)
            .with_report_title("unexpected token")
            .into_error();
    }

    let type_name = parse_identifier(ctx)?;
    let type_generics = parse_generics_list(ctx)?;

    match ctx.tokens.peek() {
        TokenKind::Assign => ctx.tokens.consume(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated type definition", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message = format!("invalid token, expected `=` (EQUAL_SIGN) but found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .with_help("add a `=` (EQUAL_SIGN) after type keyword")
                .into_error();
        }
    }

    let (type_kind, ty_position) = expect_token_type_kind(ctx)?;

    match ctx.tokens.peek() {
        TokenKind::LBrace => ctx.tokens.consume(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated type definition", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let message = format!("invalid token, expected `{{` (LEFT_BRACE) but found `{kind}`");
            return ParseIssue::new(message, ty_position)
                .with_report_title("syntax error")
                .with_help("add a `{` (LEFT_BRACE) after type kind")
                .into_error();
        }
    }

    let value = match type_kind {
        TypeDeclKind::Struct => parse_struct_definition(ctx, ty_position)?,
        TypeDeclKind::Interface => parse_interface_definition(ctx, ty_position)?,
        TypeDeclKind::Enum => parse_enum_definition(ctx, ty_position)?,
    };

    let close_brace = match ctx.tokens.peek() {
        TokenKind::RBrace => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated type definition", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_token().position;
            let message = format!("invalid token, expected `}}` (RIGHT_BRACE) but found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .with_help("did you forget a closing brace `}`?")
                .into_error();
        }
    };

    let position = type_keyword.position.merge(close_brace.position);
    Ok(AstNode::TypeDef(AstNodeTypeDef {
        visibility: NodeVisibility::Private,
        name: type_name,
        kind: type_kind,
        value: Box::new(value),
        generics: type_generics,
        position,
    }))
}

fn parse_struct_definition(ctx: &mut ParseContext<'_>, keyword_position: Span) -> Result<AstNode> {
    let mut fields = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let field_name = parse_identifier(ctx)?;
        assert!(ctx.tokens.next() == TokenKind::Colon);
        let field_type = parse_type_annotation(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::RBrace => break,
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated struct definition", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing brace `}`?")
                    .into_error();
            }
            _ => {
                let position = ctx.tokens.peek_prev_token().position;
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
    Ok(AstNode::Struct(AstNodeStruct {
        visibility: NodeVisibility::Private,
        fields,
        position,
    }))
}

fn parse_interface_definition(ctx: &mut ParseContext<'_>, start: Span) -> Result<AstNode> {
    let mut functions = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let signature = parse_function_signature(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::SemiColon => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("missing semicolon after function signature", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_prev_token().position;
                let message =
                    format!("unexpected token, expected `;` (SEMICOLON) but found `{kind}`");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .with_help("add a `;` (SEMICOLON) after function signature")
                    .into_error();
            }
        }

        functions.push(signature);
    }

    let position = start.merge(ctx.tokens.peek_token().position);
    Ok(AstNode::Interface(AstNodeInterface {
        visibility: NodeVisibility::Private,
        functions,
        position,
    }))
}

fn parse_enum_definition(ctx: &mut ParseContext<'_>, start: Span) -> Result<AstNode> {
    let mut variants = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let variant_name = parse_identifier(ctx)?;
        let variant_data = if matches!(ctx.tokens.peek(), TokenKind::LParen) {
            Some(parse_tuple_type(ctx)?)
        } else {
            None
        };

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::RBrace => break,
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated enum type definition", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing  `)`?")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_prev_token().position;
                let message = format!("unexpected token {kind}");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .with_help("did you forget to add a closing `>`?")
                    .into_error();
            }
        }

        variants.push(AstNodeEnumVariant {
            name: variant_name,
            data: variant_data,
        })
    }

    let position = start.merge(ctx.tokens.peek_token().position);
    Ok(AstNode::Enum(AstNodeEnum {
        visibility: NodeVisibility::Private,
        variants,
        position,
    }))
}

fn expect_token_type_kind(ctx: &mut ParseContext<'_>) -> Result<(TypeDeclKind, Span)> {
    let token_type_kind = ctx.tokens.next_token();

    if !matches!(
        token_type_kind.kind,
        TokenKind::Struct | TokenKind::Enum | TokenKind::Interface
    ) {
        return ParseIssue::new("this is not a valid type kind", token_type_kind.position)
            .with_report_title("unknown type kind")
            .with_help("allowed tokens here could be a `struct` or `enum`, for example")
            .into_error();
    }

    Ok((token_type_kind.kind.into(), token_type_kind.position))
}