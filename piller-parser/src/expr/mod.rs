pub mod precedence;
pub mod validation;

use piller_lexer::{Number, TokenKind};
use precedence::{OperatorLocation, PRECEDENCE_BASE, op_precedence};
use validation::{get_operator_context, get_valid_prefix_operators};

use crate::ast::*;
use crate::parser::{ParseContext, consume_optional_comma, parse_identifier};
use crate::result::{ParseIssue, Result};

fn is_valid_assignment_target(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Ident(_) | Expr::MemberAccess(_) | Expr::ArrayAccess(_)
    )
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

fn parse_postfix_expr(ctx: &mut ParseContext<'_>, mut lhs: Expr) -> Result<Expr> {
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
            Operator::LBrace => parse_struct_initialization(ctx, lhs)?,
            _ => break, // not a postfix operator we handle
        };
    }

    Ok(lhs)
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

fn parse_post_unary(ctx: &mut ParseContext<'_>, operand: Expr, op: Operator) -> Result<Expr> {
    let op_token = ctx.tokens.next_token();
    let position = operand.position().merge(op_token.position);

    Ok(Expr::Unary(UnaryExpr {
        op,
        operand: Box::new(operand),
        position,
    }))
}

fn parse_primary_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
    match ctx.tokens.peek() {
        TokenKind::Number(_) => Ok(parse_number_literal(ctx)),
        TokenKind::Bool(_) => Ok(parse_bool_literal(ctx)),
        TokenKind::String => Ok(parse_string_literal(ctx)),
        TokenKind::LBracket => parse_array_literal(ctx),
        TokenKind::Identifier => Ok(Expr::Ident(parse_identifier(ctx)?)),
        TokenKind::LBrace => parse_expr_block(ctx),
        TokenKind::Constant => parse_binding_decl(ctx, BindingMutability::Immutable),
        TokenKind::Variable => parse_binding_decl(ctx, BindingMutability::Mutable),
        TokenKind::If => parse_if_expression(ctx),
        TokenKind::SemiColon => Ok(parse_semicolon_expr(ctx)),
        TokenKind::While => parse_while_expr(ctx),
        TokenKind::For => parse_for_expr(ctx),
        t => todo!("Primary expression for {t:?}"),
    }
}

fn parse_binding_decl(ctx: &mut ParseContext<'_>, mutability: BindingMutability) -> Result<Expr> {
    use crate::parser::parse_type_annotation;

    let keyword = ctx.tokens.next_token();
    let name = parse_identifier(ctx)?;

    let ty = if matches!(ctx.tokens.peek(), TokenKind::Colon) {
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

fn parse_if_expression(ctx: &mut ParseContext<'_>) -> Result<Expr> {
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

fn parse_semicolon_expr(ctx: &mut ParseContext<'_>) -> Expr {
    let position = ctx.tokens.next_token().position;
    Expr::SemiColon(SemiColonExpr { position })
}

fn parse_while_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
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

fn parse_for_expr(ctx: &mut ParseContext<'_>) -> Result<Expr> {
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
