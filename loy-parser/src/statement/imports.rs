use loy_lexer::{DisplaySource, TokenKind};

use crate::ParseContext;
use crate::ast::*;
use crate::expr::parse_identifier;
use crate::result::{ParseIssue, Result};

pub fn parse_import(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let keyword = ctx.tokens.next_token();
    assert!(keyword.kind == TokenKind::Import);
    let path = parse_import_path(ctx)?;

    let alias = if matches!(ctx.tokens.peek(), TokenKind::As) {
        Some(parse_import_alias(ctx)?)
    } else {
        None
    };

    let methods = if matches!(ctx.tokens.peek(), TokenKind::With) {
        Some(parse_import_methods(ctx)?)
    } else {
        None
    };

    let semicolon = ctx.tokens.next_token();
    if !matches!(semicolon.kind, TokenKind::SemiColon) {
        let position = semicolon.position;
        return ParseIssue::new("missing semicolon after import definition", position)
            .with_report_title("syntax error")
            .with_help("add a ';' (SEMICOLON) at the end of import definition")
            .into_error();
    }

    let position = keyword.position.merge(semicolon.position);
    Ok(AstNode::Import(AstNodeImport {
        path,
        alias,
        methods,
        position,
    }))
}

fn parse_import_path(ctx: &mut ParseContext<'_>) -> Result<AstNodeImportPath> {
    let path_token = ctx.tokens.next_token();
    if !matches!(path_token.kind, TokenKind::String) {
        let source = path_token.display_source(ctx.source);
        let message = format!("expected path after `import` keyword, but found `{source}`");
        return ParseIssue::new(message, path_token.position)
            .with_report_title("syntax error")
            .into_error();
    }

    Ok(AstNodeImportPath {
        position: path_token.position,
    })
}

fn parse_import_alias(ctx: &mut ParseContext<'_>) -> Result<AstNodeImportAlias> {
    let as_token = ctx.tokens.next_token();
    debug_assert!(as_token.kind == TokenKind::As);
    let name = parse_identifier(ctx)?;
    let position = as_token.position.merge(name.position);
    Ok(AstNodeImportAlias { position, name })
}

fn parse_import_methods(ctx: &mut ParseContext<'_>) -> Result<AstNodeImportMethods> {
    let with_token = ctx.tokens.next_token();
    debug_assert!(with_token.kind == TokenKind::With);

    match ctx.tokens.peek() {
        TokenKind::LBrace => {
            let lbrace = ctx.tokens.next_token();
            debug_assert!(lbrace.kind == TokenKind::LBrace);

            let methods = parse_import_method_list(ctx)?;

            let rbrace = ctx.tokens.next_token();
            debug_assert!(rbrace.kind == TokenKind::RBrace);

            let position = with_token.position.merge(rbrace.position);
            Ok(AstNodeImportMethods { position, methods })
        }
        _ => {
            let methods = vec![parse_identifier(ctx)?];
            let position = with_token.position.merge(methods[0].position);
            Ok(AstNodeImportMethods { position, methods })
        }
    }
}

fn parse_import_method_list(ctx: &mut ParseContext<'_>) -> Result<Vec<IdentifierExpr>> {
    let mut methods = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let name = parse_identifier(ctx)?;
        methods.push(name);

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::RBrace => break,
            TokenKind::Eof => {
                let position = ctx.tokens.peek_token().position;
                return ParseIssue::new("unterminated import methods listing", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_token().position;
                let message = format!("unexpected token, expected identifier but found {kind}");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .into_error();
            }
        }
    }

    Ok(methods)
}
