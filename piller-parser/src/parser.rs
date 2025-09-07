use piller_lexer::{DisplaySource, Span, TokenKind, TokenStream};

use crate::ast::*;
use crate::expr::parse_expr_block;
use crate::result::{ParseIssue, ReportTitle, Result};

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
    match ctx.tokens.peek() {
        TokenKind::Type => parse_type_decl(ctx),
        TokenKind::Function => parse_function_decl(ctx),
        _ => todo!(),
    }
}

fn parse_type_decl(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let type_keyword = ctx.tokens.next_token();
    if !matches!(type_keyword.kind, TokenKind::Type) {
        let message = format!("expected keyword `type`, but found {}", type_keyword.kind);
        return ParseIssue::new(message, type_keyword.position)
            .with_report_title("unexpected token")
            .into_error();
    }

    let type_name = parse_identifier(ctx)?;
    let type_generics = parse_generics_decl(ctx)?;

    match ctx.tokens.peek() {
        TokenKind::Assign => ctx.tokens.consume(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated type declaration", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message = format!("invalid token, expected `=` (EQUAL_SIGN) but found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .with_help("add a `=` (EQUAL_SIGN) after type keyword")
                .into_error();
        }
    }

    let (type_kind, ty_position) = expect_token_type_kind(ctx)?;

    match ctx.tokens.peek() {
        TokenKind::LBrace => ctx.tokens.consume(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated type declaration", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let message = format!("invalid token, expected `{{` (LEFT_BRACE) but found `{kind}`");
            return ParseIssue::new(message, ty_position)
                .with_report_title("syntax error")
                .with_help("add a `{` (LEFT_BRACE) after type kind")
                .into_error();
        }
    }

    let value = match type_kind {
        TypeDeclKind::Struct => parse_struct_decl(ctx, ty_position)?,
        TypeDeclKind::Interface => parse_interface_decl(ctx, ty_position)?,
        TypeDeclKind::Enum => parse_enum_decl(ctx, ty_position)?,
    };

    let close_brace = match ctx.tokens.peek() {
        TokenKind::RBrace => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let position = ctx.tokens.peek_prev_token().position;
            return ParseIssue::new("unterminated type declaration", position)
                .with_report_title("unexpected end of file (EOF)")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_token().position;
            let message = format!("invalid token, expected `}}` (RIGHT_BRACE) but found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .with_help("did you forget a closing brace `}`?")
                .into_error();
        }
    };

    let position = type_keyword.position.merge(close_brace.position);
    Ok(AstNode::TypeDecl(AstNodeTypeDecl {
        name: type_name,
        kind: type_kind,
        value: Box::new(value),
        generics: type_generics,
        position,
    }))
}

fn parse_struct_decl(ctx: &mut ParseContext<'_>, keyword_position: Span) -> Result<AstNode> {
    let mut fields = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let field_name = parse_identifier(ctx)?;
        assert!(ctx.tokens.next() == TokenKind::Colon);
        let field_type = parse_type_annotation(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::RBrace => break,
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated struct declaration", position)
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

        let position = field_name.position.merge(field_type.position);
        fields.push(AstNodeStructField {
            name: field_name,
            ty: field_type,
            position,
        });
    }

    let position = keyword_position.merge(ctx.tokens.peek_token().position);
    Ok(AstNode::Struct(AstNodeStruct { fields, position }))
}

fn parse_interface_decl(ctx: &mut ParseContext<'_>, start: Span) -> Result<AstNode> {
    let mut functions = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let signature = parse_function_signature(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::SemiColon => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("missing semicolon after function signature", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_prev_token().position;
                let message =
                    format!("unexpected token, expected `;` (SEMICOLON) but found `{kind}`");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .with_help("add a `;` (SEMICOLON) after function signature")
                    .into_error();
            }
        }

        functions.push(signature);
    }

    let position = start.merge(ctx.tokens.peek_token().position);
    Ok(AstNode::Interface(AstNodeInterface {
        functions,
        position,
    }))
}

fn parse_enum_decl(ctx: &mut ParseContext<'_>, start: Span) -> Result<AstNode> {
    let mut variants = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
        let variant_name = parse_identifier(ctx)?;
        let variant_data = if matches!(ctx.tokens.peek(), TokenKind::LParen) {
            Some(parse_tuple_type(ctx)?)
        } else {
            None
        };

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::RBrace => break,
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated enum type declaration", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing  `)`?")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_prev_token().position;
                let message = format!("unexpected token {kind}");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .with_help("did you forget to add a closing `>`?")
                    .into_error();
            }
        }

        variants.push(AstNodeEnumVariant {
            name: variant_name,
            data: variant_data,
        })
    }

    let position = start.merge(ctx.tokens.peek_token().position);
    Ok(AstNode::Enum(AstNodeEnum { variants, position }))
}

pub fn parse_type_annotation(ctx: &mut ParseContext<'_>) -> Result<AstNodeTypeAnnotation> {
    if ctx.tokens.peek().is_primitive() {
        let type_node = parse_primitive_type(ctx);
        return Ok(type_node);
    }

    if matches!(ctx.tokens.peek(), TokenKind::LParen) {
        return parse_tuple_type(ctx);
    }

    let type_name = parse_identifier(ctx)?;
    let generics = parse_generics_decl(ctx)?;

    let type_kind = AstNodeTypeKind::Named(AstNodeNamedType {
        name: type_name,
        position: type_name.position,
        generics,
    });

    let type_node = AstNodeTypeAnnotation {
        kind: type_kind,
        position: type_name.position,
    };

    Ok(type_node)
}

fn parse_primitive_type(ctx: &mut ParseContext<'_>) -> AstNodeTypeAnnotation {
    debug_assert!(ctx.tokens.peek().is_primitive());
    let token_primitive = ctx.tokens.next_token();

    let primitive = match token_primitive.kind {
        TokenKind::Bool(value) => PrimitiveTypeKind::Bool(value),
        TokenKind::Integer(bit_size) => PrimitiveTypeKind::Integer(bit_size),
        TokenKind::Unsigned(bit_size) => PrimitiveTypeKind::Unsigned(bit_size),
        _ => unreachable!(),
    };

    AstNodeTypeAnnotation {
        kind: AstNodeTypeKind::Primitive(primitive),
        position: token_primitive.position,
    }
}

fn parse_generics_decl(ctx: &mut ParseContext<'_>) -> Result<Vec<AstNodeGenericDecl>> {
    let mut generics = vec![];

    if !matches!(ctx.tokens.peek(), TokenKind::Lesser) {
        return Ok(generics);
    }

    assert!(ctx.tokens.next() == TokenKind::Lesser);

    while !matches!(ctx.tokens.peek(), TokenKind::Greater | TokenKind::Eof) {
        let ty = parse_type_annotation(ctx)?;
        generics.push(AstNodeGenericDecl {
            position: ty.position,
            ty,
        });

        match ctx.tokens.peek() {
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Greater => {
                ctx.tokens.consume();
                break;
            }
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated generic declaration listing", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing  `>`?")
                    .into_error();
            }
            _ => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("generic list must be closed with a `>`", position)
                    .with_report_title("syntax error")
                    .with_help("did you forget to add a closing `>`?")
                    .into_error();
            }
        }
    }

    Ok(generics)
}

fn parse_function_decl(ctx: &mut ParseContext<'_>) -> Result<AstNode> {
    let signature = parse_function_signature(ctx)?;
    let body = parse_expr_block(ctx)?;
    Ok(AstNode::Function(signature.into_function_decl(body)))
}

fn parse_function_signature(ctx: &mut ParseContext<'_>) -> Result<AstNodeFunSignature> {
    let keyword = ctx.tokens.next_token();

    let name = parse_identifier(ctx)?;
    let generics = parse_generics_decl(ctx)?;

    match ctx.tokens.peek() {
        TokenKind::LParen => ctx.tokens.consume(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated function signature", prev_token.position)
                .with_report_title("unexpected end of file (EOF)")
                .with_help("did you forget a opening paren `(`?")
                .into_error();
        }
        kind => {
            let position = ctx.tokens.peek_prev_token().position;
            let message =
                format!("unexpected token, expected `(` (LEFT_PAREN). But found `{kind}`");
            return ParseIssue::new(message, position)
                .with_report_title("syntax error")
                .into_error();
        }
    }

    let args = parse_function_args(ctx)?;

    let rparen = match ctx.tokens.peek() {
        TokenKind::RParen => ctx.tokens.next_token(),
        TokenKind::Eof => {
            let prev_token = ctx.tokens.peek_prev_token();
            return ParseIssue::new("unterminated function signature", prev_token.position)
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

    let return_ty = if matches!(ctx.tokens.peek(), TokenKind::Colon) {
        assert!(ctx.tokens.next() == TokenKind::Colon);
        Some(parse_type_annotation(ctx)?)
    } else {
        None
    };

    let end = return_ty
        .as_ref()
        .map(|ret| ret.position)
        .unwrap_or(rparen.position);
    let position = keyword.position.merge(end);

    Ok(AstNodeFunSignature {
        name,
        args,
        generics,
        position,
        return_ty,
    })
}

fn parse_function_args(ctx: &mut ParseContext<'_>) -> Result<Vec<AstNodeFunArg>> {
    let mut args = vec![];

    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        let name = parse_identifier(ctx)?;
        assert!(ctx.tokens.next() == TokenKind::Colon);
        let ty = parse_type_annotation(ctx)?;

        match ctx.tokens.peek() {
            TokenKind::RParen => break,
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated function declaration", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing paren `)`?")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_token().position;
                let message = format!("unexpected token, expected `,` or `)`. But found `{kind}`");
                return ParseIssue::new(message, position)
                    .with_report_title("syntax error")
                    .into_error();
            }
        }

        let position = name.position.merge(ty.position);
        args.push(AstNodeFunArg { name, ty, position })
    }

    Ok(args)
}

fn expect_token_type_kind(ctx: &mut ParseContext<'_>) -> Result<(TypeDeclKind, Span)> {
    let token_type_kind = ctx.tokens.next_token();

    if !matches!(
        token_type_kind.kind,
        TokenKind::Struct | TokenKind::Enum | TokenKind::Interface
    ) {
        return ParseIssue::new("this is not a valid type kind", token_type_kind.position)
            .with_report_title("unknown type kind")
            .with_help("allowed tokens here could be a `struct` or `enum`, for example")
            .into_error();
    }

    Ok((token_type_kind.kind.into(), token_type_kind.position))
}

pub fn parse_identifier(ctx: &mut ParseContext<'_>) -> Result<IdentifierExpr> {
    let ident = ctx.tokens.next_token();

    if matches!(ident.kind, TokenKind::Eof) {
        let position = ctx.tokens.peek_prev_token().position.merge(ident.position);
        return ParseIssue::new("expected identifier for function name", position)
            .with_report_title(ReportTitle::weighted("unexpected end of file (EOF)", 1))
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

fn parse_tuple_type(ctx: &mut ParseContext<'_>) -> Result<AstNodeTypeAnnotation> {
    let lparen = ctx.tokens.next_token();
    debug_assert!(lparen.kind == TokenKind::LParen);

    let mut types = vec![];
    while !matches!(ctx.tokens.peek(), TokenKind::RParen | TokenKind::Eof) {
        types.push(parse_type_annotation(ctx)?);

        match ctx.tokens.peek() {
            TokenKind::RParen => break,
            TokenKind::Comma => ctx.tokens.consume(),
            TokenKind::Eof => {
                let position = ctx.tokens.peek_prev_token().position;
                return ParseIssue::new("unterminated tuple type", position)
                    .with_report_title("unexpected end of file (EOF)")
                    .with_help("did you forget a closing paren `)`?")
                    .into_error();
            }
            kind => {
                let position = ctx.tokens.peek_token().position;
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
            return ParseIssue::new("unterminated tuple type", position)
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
    Ok(AstNodeTypeAnnotation {
        kind: AstNodeTypeKind::Tuple(AstNodeTupleType { types, position }),
        position,
    })
}