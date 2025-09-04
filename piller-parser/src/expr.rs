use piller_lexer::{Number, TokenKind};

use crate::ast::{
    BinaryExpr, BindingExpr, BindingMutability, BlockExpr, Expr, NumberExpr, NumberKindExpr,
    Operator,
};
use crate::expect_token;
use crate::parser::{ParseContext, parse_identifier, parse_type_annotation};
use crate::result::Result;

mod prec {
    use piller_lexer::TokenKind;

    #[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    pub enum OperatorLocation {
        #[default]
        Infix,
        Prefix,
        Suffix,
    }

    pub const PRECEDENCE_BASE: u8 = 255;
    const PRECEDENCE_SUFFIX: u8 = 2;
    const PRECEDENCE_PREFIX: u8 = 3;
    const PRECEDENCE_MUL: u8 = 5;
    const PRECEDENCE_ADD: u8 = 6;
    const PRECEDENCE_COMPARISON: u8 = 9;
    const PRECEDENCE_EQUALITY: u8 = 10;
    const PRECEDENCE_BIT_AND: u8 = 11;
    const PRECEDENCE_BIT_XOR: u8 = 12;
    const PRECEDENCE_BIT_OR: u8 = 13;
    const PRECEDENCE_AND: u8 = 14;
    const PRECEDENCE_OR: u8 = 15;

    const UNREACHABLE_INFIX: &str = "operator cannot be used as an infix operator";
    const UNREACHABLE_PREFIX: &str = "operator cannot be used as an prefix operator";
    const UNREACHABLE_SUFFIX: &str = "operator cannot be used as an suffix operator";

    pub fn op_precedence(operator: TokenKind, location: OperatorLocation) -> u8 {
        use TokenKind::*;
        match operator {
            Minus | Plus => match location {
                OperatorLocation::Prefix => PRECEDENCE_PREFIX,
                OperatorLocation::Infix => PRECEDENCE_ADD,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            Not | BitNot => match location {
                OperatorLocation::Prefix => PRECEDENCE_PREFIX,
                OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            Star | Div | Mod => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_MUL,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            Increment | Decrement => match location {
                OperatorLocation::Prefix => PRECEDENCE_PREFIX,
                OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
                OperatorLocation::Suffix => PRECEDENCE_SUFFIX,
            },
            Equal | NotEqual => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_EQUALITY,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            BitAnd => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_BIT_AND,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            BitXor => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_BIT_XOR,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            BitOr => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_BIT_OR,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            And => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_AND,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            Or => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_OR,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            LParen => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
                OperatorLocation::Suffix => PRECEDENCE_SUFFIX,
            },
            Dot => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_SUFFIX,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            LBracket => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_SUFFIX,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            Lesser | Greater | LesserEqual | GreaterEqual => match location {
                OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
                OperatorLocation::Infix => PRECEDENCE_COMPARISON,
                OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
            },
            _ => unreachable!(),
        }
    }
}

pub fn parse_expression(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    parse_with_precedence(ctx, prec::PRECEDENCE_BASE)
}

fn parse_with_precedence(ctx: &mut ParseContext<'_>, precedence: u8) -> Result<Expr> {
    let mut lhs = match ctx.tokens.peek() {
        TokenKind::Constant => parse_binding_decl(ctx, BindingMutability::Immutable)?,
        TokenKind::Variable => parse_binding_decl(ctx, BindingMutability::Mutable)?,
        TokenKind::Number(_) => parse_number_literal(ctx),
        t => todo!("{t:?}"),
    };

    loop {
        let next_token = ctx.tokens.peek_token();
        if !next_token.kind.is_operator() {
            break;
        }

        let next_precedence = prec::op_precedence(next_token.kind, prec::OperatorLocation::Infix);
        if next_precedence >= precedence {
            break;
        }

        ctx.tokens.consume();
        let rhs = parse_with_precedence(ctx, next_precedence)?;
        lhs = Expr::Binary(BinaryExpr {
            position: lhs.position().merge(rhs.position()),
            op: Operator::from_token_kind(next_token.kind),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });
    }

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
