use loy_lexer::{TokenKind, TokenStream};

use crate::ast::*;
use crate::result::{ParseIssue, Result};
use crate::statement::{parse_function_definition, parse_import, parse_type_definition};

pub struct ParseContext<'src> {
    pub tokens: TokenStream,
    pub source: &'src str,
}

pub fn parse_token_stream(ctx: &mut ParseContext<'_>) -> Result<Ast> {
    let mut statements = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::Eof) {
        statements.push(parse_statement(ctx)?);
    }

    Ok(Ast::new(statements))
}

fn parse_statement(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let visibility = parse_node_visibility(ctx);

    let mut statement = match ctx.tokens.peek() {
        TokenKind::Type => parse_type_definition(ctx)?,
        TokenKind::Function => parse_function_definition(ctx)?,
        TokenKind::Import => parse_import(ctx)?,
        token => {
            let position = ctx.tokens.peek_token().position;
            return ParseIssue::new(format!("unexpected token '{token}' at top level"), position)
                .with_report_title("unexpected token")
                .with_help("expected 'type' or 'fun' declaration")
                .into_error();
        }
    };

    statement.set_visibility(visibility);
    Ok(statement)
}

fn parse_node_visibility(ctx: &mut ParseContext<'_>) -> NodeVisibility {
    if matches!(ctx.tokens.peek(), TokenKind::Pub) {
        ctx.tokens.consume();
        NodeVisibility::Public
    } else {
        NodeVisibility::Private
    }
}
