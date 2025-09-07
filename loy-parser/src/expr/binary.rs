use super::parse_expression;
use super::precedence::{OperatorLocation, PRECEDENCE_BASE, op_precedence};
use crate::ParseContext;
use crate::ast::*;
use crate::result::{ParseIssue, Result};

pub fn parse_binary_expr(ctx: &mut ParseContext<'_>, lhs: Expr) -> Result<Expr> {
    parse_with_precedence_from_lhs(ctx, lhs, PRECEDENCE_BASE)
}

fn parse_with_precedence_from_lhs(
    ctx: &mut ParseContext<'_>,
    mut lhs: Expr,
    precedence: u8,
) -> Result<Expr> {
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
        let should_break = if op.is_right_associative() {
            next_precedence > precedence // Right-associative
        } else {
            next_precedence >= precedence // Left-associative
        };
        if should_break {
            break;
        }

        ctx.tokens.consume();

        // Validate assignment targets
        if op.is_right_associative() && !is_valid_assignment_target(&lhs) {
            let position = lhs.position();
            return ParseIssue::new("invalid assignment target", position)
                .with_report_title("syntax error")
                .with_help("only identifiers, member access, and array access can be assigned to")
                .into_error();
        }

        let rhs = parse_expression(ctx)?;
        let rhs_with_higher_precedence = parse_with_precedence_from_lhs(ctx, rhs, next_precedence)?;

        lhs = Expr::Binary(BinaryExpr {
            position: lhs.position().merge(rhs_with_higher_precedence.position()),
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs_with_higher_precedence),
        });
    }

    Ok(lhs)
}

fn is_valid_assignment_target(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Ident(_) | Expr::MemberAccess(_) | Expr::ArrayAccess(_)
    )
}