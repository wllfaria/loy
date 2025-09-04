pub mod precedence;
pub mod validation;

use piller_lexer::Number;
use piller_lexer::TokenKind;
use precedence::OperatorLocation;
use precedence::PRECEDENCE_BASE;
use precedence::op_precedence;
use validation::get_operator_context;
use validation::get_valid_prefix_operators;

use crate::ast::*;
use crate::expect_token;
use crate::parser::ParseContext;
use crate::parser::consume_optional_comma;
use crate::parser::parse_identifier;
use crate::result::Result;

pub fn parse_expr_block(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    use crate::expect_token;
    use piller_lexer::TokenKind;

    let mut exprs = vec![];
    let block_start = expect_token!(ctx.tokens, TokenKind::LBrace)?;

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        exprs.push(parse_expression(ctx)?);
    }

    let block_end = expect_token!(ctx.tokens, TokenKind::RBrace)?;
    let position = block_start.position.merge(block_end.position);
    Ok(AstNode::Block(crate::ast::AstNodeBlock { exprs, position }))
}

pub fn parse_expression(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let primary = parse_prefix_expr(ctx)?;
    let postfix = parse_postfix_expr(ctx, primary)?;
    let binary = parse_binary_expr(ctx, postfix)?;
    Ok(binary)
}

fn parse_prefix_expr(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let valid_prefix_ops = get_valid_prefix_operators();

    let next_token = ctx.tokens.peek();
    if next_token.is_operator() {
        let op = Operator::from_token_kind(next_token);

        if valid_prefix_ops.contains(&op) {
            let op_token = ctx.tokens.next_token();
            let operand = parse_prefix_expr(ctx)?;
            let position = op_token.position.merge(operand.position());

            return Ok(AstNode::Unary(AstNodeUnaryExpr {
                op,
                operand: Box::new(operand),
                position,
            }));
        }
    }

    parse_primary_expr(ctx)
}

fn parse_postfix_expr(ctx: &mut ParseContext<'_>, mut lhs: AstNode) -> Result<AstNode> {
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
            Operator::Increment | Operator::Decrement => parse_post_unary(ctx, lhs, op)?,
            _ => break, // not a postfix operator we handle
        };
    }

    Ok(lhs)
}

fn parse_function_call(ctx: &mut ParseContext<'_>, callee: AstNode) -> Result<AstNode> {
    expect_token!(ctx.tokens, TokenKind::LParen)?;
    let mut args = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        args.push(parse_expression(ctx)?);
        consume_optional_comma(ctx);
    }

    let rparen = expect_token!(ctx.tokens, TokenKind::RParen)?;
    let position = callee.position().merge(rparen.position);

    Ok(AstNode::FunctionCall(AstNodeFunctionCall {
        callee: Box::new(callee),
        args,
        position,
    }))
}

fn parse_array_access(ctx: &mut ParseContext<'_>, array: AstNode) -> Result<AstNode> {
    expect_token!(ctx.tokens, TokenKind::LBracket)?;
    let index = parse_expression(ctx)?;
    let rbracket = expect_token!(ctx.tokens, TokenKind::RBracket)?;
    let position = array.position().merge(rbracket.position);

    Ok(AstNode::ArrayAccess(AstNodeArrayAccess {
        array: Box::new(array),
        index: Box::new(index),
        position,
    }))
}

fn parse_member_access(ctx: &mut ParseContext<'_>, object: AstNode) -> Result<AstNode> {
    expect_token!(ctx.tokens, TokenKind::Dot)?;
    let member = parse_identifier(ctx)?;
    let position = object.position().merge(member.position);

    Ok(AstNode::MemberAccess(AstNodeMemberAccess {
        object: Box::new(object),
        member,
        position,
    }))
}

fn parse_post_unary(ctx: &mut ParseContext<'_>, operand: AstNode, op: Operator) -> Result<AstNode> {
    let op_token = ctx.tokens.next_token();
    let position = operand.position().merge(op_token.position);

    Ok(AstNode::Unary(AstNodeUnaryExpr {
        op,
        operand: Box::new(operand),
        position,
    }))
}

fn parse_primary_expr(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    match ctx.tokens.peek() {
        TokenKind::Number(_) => Ok(parse_number_literal(ctx)),
        TokenKind::Identifier => Ok(AstNode::Ident(parse_identifier(ctx)?)),
        TokenKind::LBrace => parse_expr_block(ctx),
        TokenKind::Constant => parse_binding_decl(ctx, BindingMutability::Immutable),
        TokenKind::Variable => parse_binding_decl(ctx, BindingMutability::Mutable),
        t => todo!("Primary expression for {t:?}"),
    }
}

fn parse_binding_decl(
    ctx: &mut ParseContext<'_>,
    mutability: BindingMutability,
) -> Result<AstNode> {
    use crate::parser::parse_type_annotation;

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

    Ok(AstNode::Binding(AstNodeBinding {
        ty,
        name,
        position,
        mutability,
        value: Box::new(value),
    }))
}

fn parse_number_literal(ctx: &mut ParseContext<'_>) -> AstNode {
    let next_token = ctx.tokens.next_token();
    let TokenKind::Number(number) = next_token.kind else { unreachable!() };

    match number {
        Number::Unsigned(n) => AstNode::Number(AstNodeNumber {
            kind: NumberKindExpr::Unsigned(n),
            position: next_token.position,
        }),
        Number::Signed(n) => AstNode::Number(AstNodeNumber {
            kind: NumberKindExpr::Signed(n),
            position: next_token.position,
        }),
        Number::Float(n) => AstNode::Number(AstNodeNumber {
            kind: NumberKindExpr::Float(n),
            position: next_token.position,
        }),
    }
}

pub fn parse_binary_expr(ctx: &mut ParseContext<'_>, lhs: AstNode) -> Result<AstNode> {
    parse_with_precedence_from_lhs(ctx, lhs, PRECEDENCE_BASE)
}

fn parse_with_precedence_from_lhs(
    ctx: &mut ParseContext<'_>,
    mut lhs: AstNode,
    precedence: u8,
) -> Result<AstNode> {
    loop {
        let next_token = ctx.tokens.peek_token();
        if !next_token.kind.is_operator() {
            break;
        }

        let op = Operator::from_token_kind(next_token.kind);
        // skip non-binary non-infix operators
        if !op.is_binary() {
            break;
        }

        let next_precedence = op_precedence(op, OperatorLocation::Infix);
        if next_precedence >= precedence {
            break;
        }

        ctx.tokens.consume();
        let rhs = parse_expression(ctx)?;
        let rhs_with_higher_precedence = parse_with_precedence_from_lhs(ctx, rhs, next_precedence)?;

        lhs = AstNode::Binary(AstNodeBinaryExpr {
            position: lhs.position().merge(rhs_with_higher_precedence.position()),
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs_with_higher_precedence),
        });
    }

    Ok(lhs)
}
