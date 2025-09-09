use loy_ast::ast::*;
use loy_ast::result::{ParseIssue, Result};
use loy_ast::token::{DisplaySource, Number, TokenKind};

use super::control_flow::*;
use super::*;
use crate::ParseContext;

pub fn parse_primary_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    match ctx.tokens.peek() {
        TokenKind::Number(_) => Ok(parse_number_literal(ctx)),
        TokenKind::Bool(_) => Ok(parse_bool_literal(ctx)),
        TokenKind::String => Ok(parse_string_literal(ctx)),
        TokenKind::LBracket => parse_array_literal(ctx),
        TokenKind::LParen => parse_tuple_or_parenthesized_expr(ctx),
        TokenKind::Identifier => Ok(Expr::Ident(parse_identifier(ctx)?)),
        TokenKind::LBrace => parse_expr_block(ctx),
        TokenKind::Constant => parse_binding_decl(ctx, BindingMutability::Immutable),
        TokenKind::Variable => parse_binding_decl(ctx, BindingMutability::Mutable),
        TokenKind::If => parse_if_expression(ctx),
        TokenKind::SemiColon => Ok(parse_semicolon_expr(ctx)),
        TokenKind::While => parse_while_expr(ctx),
        TokenKind::For => parse_for_expr(ctx),
        TokenKind::Return => parse_return_expr(ctx),
        TokenKind::Break => parse_break_expr(ctx),
        TokenKind::Continue => parse_continue_expr(ctx),
        TokenKind::Comment => Ok(parse_comment(ctx)),
        TokenKind::DocComment => Ok(parse_doc_comment(ctx)),
        token => {
            let position = ctx.tokens.peek_token().position;
            let message = format!("unexpected token '{token}' in primary expression");
            ParseIssue::new(message, position)
                .with_report_title("unexpected token")
                .with_help("expected an expression like a number, identifier, string, or '('")
                .into_error()
        }
    }
}

fn parse_number_literal(ctx: &mut ParseContext<'_>) -> Expr {
    let next_token = ctx.tokens.next_token();
    let TokenKind::Number(number) = next_token.kind else { unreachable!() };

    match number {
        Number::Unsigned(n) => Expr::Number(NumberExpr {
            kind: NumberKindExpr::Unsigned(n),
            position: next_token.position,
        }),
        Number::Signed(n) => Expr::Number(NumberExpr {
            kind: NumberKindExpr::Signed(n),
            position: next_token.position,
        }),
        Number::Float(n) => Expr::Number(NumberExpr {
            kind: NumberKindExpr::Float(n),
            position: next_token.position,
        }),
    }
}

fn parse_bool_literal(ctx: &mut ParseContext<'_>) -> Expr {
    let next_token = ctx.tokens.next_token();
    let TokenKind::Bool(value) = next_token.kind else { unreachable!() };
    Expr::Bool(BoolExpr {
        value,
        position: next_token.position,
    })
}

fn parse_string_literal(ctx: &mut ParseContext<'_>) -> Expr {
    let next_token = ctx.tokens.next_token();
    let TokenKind::String = next_token.kind else { unreachable!() };

    Expr::String(StringExpr {
        position: next_token.position,
    })
}

fn parse_array_literal(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let lbracket = ctx.tokens.next_token();
    assert!(lbracket.kind == TokenKind::LBracket);
    let mut elements = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBracket | TokenKind::Eof) {
        elements.push(parse_expression(ctx)?);
        consume_optional_comma(ctx);
    }

    let rbracket = match ctx.tokens.peek() {
        TokenKind::RBracket => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated array literal", prev_token.position)
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
    let position = lbracket.position.merge(rbracket.position);
    Ok(Expr::Array(ArrayExpr { elements, position }))
}

fn parse_tuple_or_parenthesized_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let lparen = ctx.tokens.next_token();
    debug_assert!(lparen.kind == TokenKind::LParen);

    // no expression inside of parenthesis, this is a unit value `()` (empty tuple)
    if matches!(ctx.tokens.peek(), TokenKind::RParen) {
        return Ok(Expr::Tuple(TupleExpr {
            elements: vec![],
            position: lparen.position.merge(ctx.tokens.next_token().position),
        }));
    }

    let first_expr = parse_expression(ctx)?;

    // simply a parenthesized expression (expr), not a tuple
    if matches!(ctx.tokens.peek(), TokenKind::RParen) {
        ctx.tokens.consume();
        return Ok(first_expr);
    }

    if matches!(ctx.tokens.peek(), TokenKind::Eof) {
        let position = ctx.tokens.peek_prev_token().position;
        return ParseIssue::new("unterminated parenthesized expression", position)
            .with_report_title("unexpected end of file (EOF)")
            .with_help("did you forget a closing paren `)`?")
            .into_error();
    }

    // if the expression is not immediately closed or invalid due to EOF, then it can either be a
    // tuple or an invalid syntax.
    if !matches!(ctx.tokens.peek(), TokenKind::Comma) {
        let kind = ctx.tokens.peek();
        let position = ctx.tokens.peek_prev_token().position;
        let message = format!("unexpected token, expected `,` or `)`. But found `{kind}`");
        return ParseIssue::new(message, position)
            .with_report_title("syntax error")
            .into_error();
    }

    debug_assert!(ctx.tokens.peek() == TokenKind::Comma);
    ctx.tokens.consume(); // consume comma

    let mut elements = vec![first_expr];

    // since we found a comma after the first expressionm, this is a tuple and a comma separated
    // list of expressions should be parsed
    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        elements.push(parse_expression(ctx)?);

        match ctx.tokens.peek() {
            TokenKind::RParen => break,
            TokenKind::Comma => consume_optional_comma(ctx),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated tuple literal", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing paren `)`?")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_prev_token().position;
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
            return ParseIssue::new("unterminated tuple literal", position)
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
    Ok(Expr::Tuple(TupleExpr { elements, position }))
}

pub fn parse_identifier(ctx: &mut ParseContext<'_>) -> Result<IdentifierExpr> {
    let ident = ctx.tokens.next_token();

    if matches!(ident.kind, TokenKind::Eof) {
        let position = ctx.tokens.peek_prev_token().position.merge(ident.position);
        return ParseIssue::new("expected identifier for function name", position)
            .with_report_title("unexpected end of file (EOF)")
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
    Ok(IdentifierExpr { position })
}

fn parse_comment(ctx: &mut ParseContext<'_>) -> Expr {
    let comment_token = ctx.tokens.next_token();
    debug_assert!(comment_token.kind == TokenKind::Comment);
    Expr::Comment(CommentExpr {
        position: comment_token.position,
    })
}

fn parse_doc_comment(ctx: &mut ParseContext<'_>) -> Expr {
    let comment_token = ctx.tokens.next_token();
    debug_assert!(comment_token.kind == TokenKind::DocComment);
    Expr::DocComment(DocCommentExpr {
        position: comment_token.position,
    })
}