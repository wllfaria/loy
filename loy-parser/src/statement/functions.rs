use loy_ast::ast::*;
use loy_ast::token::TokenKind;

use super::types::{parse_generics_list, parse_type_annotation};
use crate::ParseContext;
use crate::expr::{parse_expr_block, parse_identifier};
use crate::result::{ParseIssue, Result};

pub fn parse_function_definition(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let signature = parse_function_signature(ctx)?;
    let body = parse_expr_block(ctx)?;
    Ok(AstNode::Function(Box::new(
        signature.into_function_decl(body),
    )))
}

pub fn parse_function_signature(ctx: &mut ParseContext<'_>) -> Result<AstNodeFunSignature> {
    let keyword = ctx.tokens.next_token();

    let name = parse_identifier(ctx)?;
    let generics = parse_generics_list(ctx)?;

    match ctx.tokens.peek() {
        TokenKind::LParen => ctx.tokens.consume(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated function signature", prev_token.position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("did you forget a opening paren `(`?")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message =
                format!("unexpected token, expected `(` (LEFT_PAREN). But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .into_error();
        }
    }

    let args = parse_function_arguments(ctx)?;

    let rparen = match ctx.tokens.peek() {
        TokenKind::RParen => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated function signature", prev_token.position)
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

    let return_ty = if matches!(ctx.tokens.peek(), TokenKind::Colon) {
        assert!(ctx.tokens.next() == TokenKind::Colon);
        Some(parse_type_annotation(ctx)?)
    } else {
        None
    };

    let end = return_ty
        .as_ref()
        .map(|ret| ret.position)
        .unwrap_or(rparen.position);
    let position = keyword.position.merge(end);

    Ok(AstNodeFunSignature {
        name,
        args,
        generics,
        position,
        return_ty,
    })
}

fn parse_function_arguments(ctx: &mut ParseContext<'_>) -> Result<Vec<AstNodeFunArg>> {
    let mut args = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        let name = parse_identifier(ctx)?;
        assert!(ctx.tokens.next() == TokenKind::Colon);
        let ty = parse_type_annotation(ctx)?;
        let position = name.position.merge(ty.position);
        args.push(AstNodeFunArg { name, ty, position });

        match ctx.tokens.peek() {
            TokenKind::RParen => break,
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated function definition", position)
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

    Ok(args)
}
