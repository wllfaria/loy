use loy_ast::ast::*;
use loy_ast::result::{ParseIssue, Result};
use loy_ast::token::TokenKind;

use super::validation::get_operator_context;
use super::{consume_optional_comma, parse_expression, parse_identifier};
use crate::ParseContext;

pub fn parse_postfix_expr(ctx: &mut ParseContext<'_>, mut lhs: Expr) -> Result<Expr> {
    loop {
        let validation = get_operator_context(&lhs);
        let next_token = ctx.tokens.peek();
        if !next_token.is_operator() {
            break;
        }

        let op = Operator::from_token_kind(next_token);
        if !validation.valid_postfix.contains(&op) {
            break;
        }

        lhs = match op {
            Operator::LParen => parse_function_call(ctx, lhs)?,
            Operator::LBracket => parse_array_access(ctx, lhs)?,
            Operator::Dot => parse_member_access(ctx, lhs)?,
            Operator::Increment | Operator::Decrement => parse_postfix_unary(ctx, lhs, op)?,
            Operator::LBrace => parse_struct_initialization(ctx, lhs)?,
            _ => break, // not a postfix operator we handle
        };
    }

    Ok(lhs)
}

fn parse_postfix_unary(ctx: &mut ParseContext<'_>, operand: Expr, op: Operator) -> Result<Expr> {
    let op_token = ctx.tokens.next_token();
    let position = operand.position().merge(op_token.position);

    Ok(Expr::Unary(UnaryExpr {
        op,
        operand: Box::new(operand),
        position,
    }))
}

fn parse_function_call(ctx: &mut ParseContext<'_>, callee: Expr) -> Result<Expr> {
    assert!(ctx.tokens.next() == TokenKind::LParen);
    let mut args = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        args.push(parse_expression(ctx)?);
        consume_optional_comma(ctx);
    }

    if matches!(ctx.tokens.peek(), TokenKind::Eof) {
        let prev_token = ctx.tokens.peek_prev_token();
        return ParseIssue::new("unterminated function call", prev_token.position)
            .with_report_title("unexpected end of file (EOF)")
            .with_help("did you forget a closing paren `)`?")
            .into_error();
    }

    let rparen = match ctx.tokens.peek() {
        TokenKind::RParen => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated function call", prev_token.position)
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

    let position = callee.position().merge(rparen.position);
    Ok(Expr::FunctionCall(FunctionCallExpr {
        callee: Box::new(callee),
        args,
        position,
    }))
}

fn parse_array_access(ctx: &mut ParseContext<'_>, array: Expr) -> Result<Expr> {
    assert!(ctx.tokens.next() == TokenKind::LBracket);
    let index = parse_expression(ctx)?;

    let rbracket = match ctx.tokens.peek() {
        TokenKind::RBracket => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated array index access", prev_token.position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("did you forget a closing bracket `]`?")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message =
                format!("unexpected token, expected `]` (RIGHT_BRACKET). But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .into_error();
        }
    };

    let position = array.position().merge(rbracket.position);
    Ok(Expr::ArrayAccess(ArrayAccessExpr {
        array: Box::new(array),
        index: Box::new(index),
        position,
    }))
}

fn parse_member_access(ctx: &mut ParseContext<'_>, object: Expr) -> Result<Expr> {
    assert!(ctx.tokens.next() == TokenKind::Dot);
    let member = parse_identifier(ctx)?;
    let position = object.position().merge(member.position);

    Ok(Expr::MemberAccess(MemberAccessExpr {
        object: Box::new(object),
        member,
        position,
    }))
}

fn parse_struct_initialization(ctx: &mut ParseContext<'_>, lhs: Expr) -> Result<Expr> {
    assert!(ctx.tokens.next() == TokenKind::LBrace);
    let mut fields = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let field_name = parse_identifier(ctx)?;
        match ctx.tokens.peek() {
            TokenKind::Assign => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.prev_token().position;
                return ParseIssue::new("unterminated struct initialization", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_prev_token().position;
                let message =
                    format!("unexpected token, expected `=` (EQUAL_SIGN). But found `{kind}`");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .into_error();
            }
        }
        let field_value = parse_expression(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::Comma => consume_optional_comma(ctx),
            TokenKind::RBrace => break,
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated struct initialization", position)
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

        let position = field_name.position.merge(field_value.position());
        fields.push(StructFieldInitExpr {
            field_name,
            field_value: Box::new(field_value),
            position,
        })
    }

    let rbrace = match ctx.tokens.peek() {
        TokenKind::RBrace => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated struct initialization", prev_token.position)
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
                .into_error();
        }
    };

    let position = lhs.position().merge(rbrace.position);
    Ok(Expr::StructInit(StructInitExpr { fields, position }))
}
