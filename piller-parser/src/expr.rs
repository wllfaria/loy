use piller_lexer::{Number, TokenKind};

use crate::ast::{BindingExpr, BindingMutability, BlockExpr, Expr, NumberExpr, NumberKindExpr};
use crate::expect_token;
use crate::parser::{ParseContext, parse_identifier, parse_type_annotation};
use crate::result::Result;

mod prec {
    pub const BASE_PRECEDENCE: u8 = 0;
}

pub fn parse_expression(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    parse_with_precedence(ctx, prec::BASE_PRECEDENCE)
}

fn parse_with_precedence(ctx: &mut ParseContext<'_>, _precedence: u8) -> Result<Expr> {
    let lhs = match ctx.tokens.peek() {
        TokenKind::Constant => parse_binding_decl(ctx, BindingMutability::Immutable)?,
        TokenKind::Variable => parse_binding_decl(ctx, BindingMutability::Mutable)?,
        TokenKind::Number(_) => parse_number_literal(ctx),
        t => todo!("{t:?}"),
    };

    Ok(lhs)
}

pub fn parse_expr_block(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    let mut exprs = vec![];

    let block_start = expect_token!(ctx.tokens, TokenKind::LBrace)?;

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        exprs.push(parse_expression(ctx)?);
    }

    let block_end = expect_token!(ctx.tokens, TokenKind::RBrace)?;
    let position = block_start.position.merge(block_end.position);
    Ok(Expr::Block(BlockExpr { exprs, position }))
}

fn parse_binding_decl(ctx: &mut ParseContext<'_>, mutability: BindingMutability) -> Result<Expr> {
    let keyword = ctx.tokens.next_token();
    let name = parse_identifier(ctx)?;

    let ty = if matches!(ctx.tokens.peek(), TokenKind::Colon) {
        Some(parse_type_annotation(ctx)?)
    } else {
        None
    };

    expect_token!(ctx.tokens, TokenKind::Assign)?;
    let value = parse_expression(ctx)?;
    let position = keyword.position.merge(value.position());
    expect_token!(ctx.tokens, TokenKind::SemiColon)?;

    Ok(Expr::Binding(BindingExpr {
        ty,
        name,
        position,
        mutability,
        value: Box::new(value),
    }))
}

fn parse_number_literal(ctx: &mut ParseContext<'_>) -> Expr {
    let next_token = ctx.tokens.next_token();
    let TokenKind::Number(number) = next_token.kind else {
        unreachable!();
    };

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