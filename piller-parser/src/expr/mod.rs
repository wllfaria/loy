mod binary;
mod control_flow;
mod postfix;
mod precedence;
mod primary;
mod validation;

use binary::parse_binary_expr;
use piller_lexer::TokenKind;
use postfix::parse_postfix_expr;
pub use primary::parse_identifier;
use primary::parse_primary_expr;
use validation::get_valid_prefix_operators;

use crate::ast::*;
use crate::parser::ParseContext;
use crate::result::{ParseIssue, Result};
use crate::statement::parse_type_annotation;

pub fn parse_expression(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let primary = parse_prefix_expr(ctx)?;
    let postfix = parse_postfix_expr(ctx, primary)?;
    let binary = parse_binary_expr(ctx, postfix)?;
    Ok(binary)
}

fn parse_prefix_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let valid_prefix_ops = get_valid_prefix_operators();

    let next_token = ctx.tokens.peek();
    if next_token.is_operator() {
        let op = Operator::from_token_kind(next_token);

        if valid_prefix_ops.contains(&op) {
            let op_token = ctx.tokens.next_token();
            let operand = parse_prefix_expr(ctx)?;
            let position = op_token.position.merge(operand.position());

            return Ok(Expr::Unary(UnaryExpr {
                op,
                operand: Box::new(operand),
                position,
            }));
        }
    }

    parse_primary_expr(ctx)
}

fn parse_semicolon_expr(ctx: &mut ParseContext<'_>) -> Expr {
    let position = ctx.tokens.next_token().position;
    Expr::SemiColon(SemiColonExpr { position })
}

fn parse_binding_decl(ctx: &mut ParseContext<'_>, mutability: BindingMutability) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();
    let name = parse_identifier(ctx)?;

    let ty = if matches!(ctx.tokens.peek(), TokenKind::Colon) {
        ctx.tokens.consume();
        Some(parse_type_annotation(ctx)?)
    } else {
        None
    };

    match ctx.tokens.peek() {
        TokenKind::Assign => ctx.tokens.consume(),
        TokenKind::Eof if matches!(mutability, BindingMutability::Immutable) => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated constant declaration", position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("constant syntax is `const <identifier> = <expression>;")
                .into_error();
        }
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated variable declaration", position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("variable syntax is `var <identifier> = <expression>;")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message = format!("invalid token, expected `=` (EQUAL_SIGN) but found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .with_help("add a `=` (EQUAL_SIGN) before binding value")
                .into_error();
        }
    }

    let value = parse_expression(ctx)?;
    let position = keyword.position.merge(value.position());
    match ctx.tokens.peek() {
        TokenKind::SemiColon => ctx.tokens.consume(),
        _ => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("missing semicolon after binding declaration", position)
                .with_report_title("syntax error")
                .with_help("add a `;` (SEMICOLON) after binding value")
                .into_error();
        }
    };

    Ok(Expr::Binding(BindingExpr {
        ty,
        name,
        position,
        mutability,
        value: Box::new(value),
    }))
}

pub fn parse_expr_block(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let mut exprs = vec![];
    let block_start = match ctx.tokens.peek() {
        TokenKind::LBrace => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let position = ctx.tokens.prev_token().position;
            return ParseIssue::new("unterminated expression block", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message =
                format!("unexpected token, expected `{{` (LEFT_BRACE). But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .into_error();
        }
    };

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let expression = parse_expression(ctx)?;
        exprs.push(expression);
    }

    let block_end = match ctx.tokens.peek() {
        TokenKind::RBrace => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unclosed expression block", position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("did you forget a closing brace `}`?")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message =
                format!("unexpected token, expected `}}` (RIGHT_BRACE). But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .with_help("did you forget a closing brace `}`?")
                .into_error();
        }
    };

    let position = block_start.position.merge(block_end.position);
    Ok(Expr::Block(BlockExpr { exprs, position }))
}

pub fn consume_optional_comma(ctx: &mut ParseContext<'_>) {
    if matches!(ctx.tokens.peek(), TokenKind::Comma) {
        ctx.tokens.consume();
    }
}