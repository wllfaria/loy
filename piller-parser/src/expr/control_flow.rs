use piller_lexer::TokenKind;

use super::{parse_expr_block, parse_expression, parse_identifier};
use crate::ParseContext;
use crate::ast::*;
use crate::result::{ParseIssue, Result};

pub fn parse_if_expression(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();
    let condition = parse_condition_expression(ctx)?;
    let truthy = parse_expr_block(ctx)?;

    let falsy = if matches!(ctx.tokens.peek(), TokenKind::Else) {
        // consume the else token
        let else_position = ctx.tokens.next_token().position;
        let next_token = ctx.tokens.peek_token();
        let error_span = else_position.merge(next_token.position);

        match next_token.kind {
            TokenKind::If => Some(Box::new(parse_if_expression(ctx)?)),
            TokenKind::LBrace => Some(Box::new(parse_expr_block(ctx)?)),
            _ => ParseIssue::new("expected `if` or block after `else`", error_span)
                .with_report_title("syntax error")
                .into_error()?,
        }
    } else {
        None
    };

    let position = match falsy.as_ref() {
        Some(falsy) => keyword.position.merge(falsy.position()),
        None => keyword.position.merge(truthy.position()),
    };

    Ok(Expr::If(IfExpr {
        condition: Box::new(condition),
        truthy: Box::new(truthy),
        falsy,
        position,
    }))
}

pub fn parse_while_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();
    let condition = parse_condition_expression(ctx)?;
    let body = parse_expr_block(ctx)?;
    let position = keyword.position.merge(body.position());

    Ok(Expr::While(WhileExpr {
        position,
        condition: Box::new(condition),
        body: Box::new(body),
    }))
}

pub fn parse_for_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();
    let first_ident = parse_identifier(ctx)?;

    let (index_var, item_var) = if matches!(ctx.tokens.peek(), TokenKind::Comma) {
        assert!(ctx.tokens.next() == TokenKind::Comma);
        let second_ident = parse_identifier(ctx)?;
        (Some(first_ident), second_ident)
    } else {
        (None, first_ident)
    };

    match ctx.tokens.peek() {
        TokenKind::In => ctx.tokens.consume(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated for loop", prev_token.position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message = format!("unexpected token, expected `in` keyword. But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .into_error();
        }
    }

    let iterable = parse_expression(ctx)?;
    let body = parse_expr_block(ctx)?;
    let position = keyword.position.merge(body.position());

    Ok(Expr::For(ForExpr {
        index_var,
        item_var,
        iterable: Box::new(iterable),
        body: Box::new(body),
        position,
    }))
}

pub fn parse_return_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();

    let value = match ctx.tokens.peek() {
        TokenKind::SemiColon => None,
        _ => Some(Box::new(parse_expression(ctx)?)),
    };

    let position = match &value {
        Some(expr) => keyword.position.merge(expr.position()),
        None => keyword.position,
    };

    match ctx.tokens.peek() {
        TokenKind::SemiColon => ctx.tokens.consume(),
        _ => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("missing semicolon after return statement", position)
                .with_report_title("syntax error")
                .with_help("add a `;` (SEMICOLON) after return statement")
                .into_error();
        }
    };

    Ok(Expr::Return(ReturnExpr { value, position }))
}

pub fn parse_break_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();

    let value = match ctx.tokens.peek() {
        TokenKind::SemiColon => None,
        _ => Some(Box::new(parse_expression(ctx)?)),
    };

    let position = match &value {
        Some(expr) => keyword.position.merge(expr.position()),
        None => keyword.position,
    };

    match ctx.tokens.peek() {
        TokenKind::SemiColon => ctx.tokens.consume(),
        _ => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("missing semicolon after break statement", position)
                .with_report_title("syntax error")
                .with_help("add a `;` (SEMICOLON) after break statement")
                .into_error();
        }
    };

    Ok(Expr::Break(BreakExpr { value, position }))
}

pub fn parse_continue_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();
    let position = keyword.position;

    match ctx.tokens.peek() {
        TokenKind::SemiColon => ctx.tokens.consume(),
        _ => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("missing semicolon after continue statement", position)
                .with_report_title("syntax error")
                .with_help("add a `;` (SEMICOLON) after continue statement")
                .into_error();
        }
    };

    Ok(Expr::Continue(ContinueExpr { position }))
}

fn parse_condition_expression(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let expr = parse_expression(ctx)?;

    match expr {
        Expr::Block(_) => ParseIssue::new("block cannot be used as condition", expr.position())
            .with_report_title("syntax error")
            .with_help("a condition must be an expression that evaluates to a boolean value")
            .into_error()?,
        _ => Ok(expr),
    }
}