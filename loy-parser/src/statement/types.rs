use loy_lexer::TokenKind;

use crate::ParseContext;
use crate::ast::*;
use crate::expr::parse_identifier;
use crate::result::{ParseIssue, Result};

pub fn parse_type_annotation(ctx: &mut ParseContext<'_>) -> Result<AstNodeTypeAnnotation> {
    if ctx.tokens.peek().is_primitive() {
        let type_node = parse_primitive_type(ctx);
        return Ok(type_node);
    }

    if matches!(ctx.tokens.peek(), TokenKind::LParen) {
        return parse_tuple_type(ctx);
    }

    let type_name = parse_identifier(ctx)?;
    let generics = parse_generics_list(ctx)?;

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

pub fn parse_generics_list(ctx: &mut ParseContext<'_>) -> Result<Vec<AstNodeGenericDecl>> {
    let mut generics = vec![];

    if !matches!(ctx.tokens.peek(), TokenKind::Lesser) {
        return Ok(generics);
    }

    assert!(ctx.tokens.next() == TokenKind::Lesser);

    while !matches!(ctx.tokens.peek(), TokenKind::Greater | TokenKind::Eof) {
        let ty = parse_type_annotation(ctx)?;
        generics.push(AstNodeGenericDecl {
            position: ty.position,
            ty,
        });

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Greater => {
                ctx.tokens.consume();
                break;
            }
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated generic declaration listing", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing  `>`?")
                    .into_error();
            }
            _ => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("generic list must be closed with a `>`", position)
                    .with_report_title("syntax error")
                    .with_help("did you forget to add a closing `>`?")
                    .into_error();
            }
        }
    }

    Ok(generics)
}

pub fn parse_tuple_type(ctx: &mut ParseContext<'_>) -> Result<AstNodeTypeAnnotation> {
    let lparen = ctx.tokens.next_token();
    debug_assert!(lparen.kind == TokenKind::LParen);

    let mut types = vec![];
    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        types.push(parse_type_annotation(ctx)?);

        match ctx.tokens.peek() {
            TokenKind::RParen => break,
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated tuple type", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing paren `)`?")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_token().position;
                let message = format!("unexpected token, expected `,` or `)`. But found `{kind}`");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .into_error();
            }
        }
    }

    let rparen = match ctx.tokens.peek() {
        TokenKind::RParen => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated tuple type", position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("did you forget a closing paren `)`?")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message =
                format!("unexpected token, expected `)` (RIGHT_PAREN). But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .into_error();
        }
    };

    let position = lparen.position.merge(rparen.position);
    Ok(AstNodeTypeAnnotation {
        kind: AstNodeTypeKind::Tuple(AstNodeTupleType { types, position }),
        position,
    })
}