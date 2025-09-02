use piller_lexer::{Span, TokenKind, TokenStream};

use crate::ast::{
    Ast, AstNode, AstNodeGenericDecl, AstNodeIdentifier, AstNodeNamedType, AstNodeStruct,
    AstNodeStructField, AstNodeTypeAnnotation, AstNodeTypeDecl, AstNodeTypeKind, PrimitiveTypeKind,
    TypeDeclKind,
};
use crate::result::{ParseIssue, ParseOutput, Result};

macro_rules! expect_token {
    ($tokens:expr, $($pat:pat),+ $(,)?) => {{
        let tok = $tokens.next_token();
        if matches!(tok.kind, $($pat)|+) {
            ParseOutput::with_value(tok)
        } else {
            ParseOutput::with_issue(ParseIssue::new(
                concat!("expected one of: ", $(stringify!($pat), " "),+),
                tok.position,
            ))
        }
    }};
}

#[derive(Debug, Default)]
pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(&self, mut tokens: TokenStream) -> Result<Ast> {
        let mut statements = vec![];

        while !matches!(tokens.peek(), TokenKind::Eof) {
            let statement = self
                .parse_statement(&mut tokens)
                .into_result("parser error");

            let statement = statement?;

            statements.push(statement);
        }

        Ok(Ast::new(statements))
    }

    fn parse_statement(&self, tokens: &mut TokenStream) -> ParseOutput<AstNode> {
        match tokens.peek() {
            TokenKind::Type => self.parse_type_decl(tokens),
            TokenKind::If => todo!(),
            TokenKind::Else => todo!(),
            TokenKind::Struct => todo!(),
            TokenKind::Enum => todo!(),
            TokenKind::Constant => todo!(),
            TokenKind::Function => todo!(),
            TokenKind::Variable => todo!(),
            TokenKind::String => todo!(),
            TokenKind::Bool(_) => todo!(),
            TokenKind::Number(_) => todo!(),
            TokenKind::Integer(_) => todo!(),
            TokenKind::Unsigned(_) => todo!(),
            TokenKind::Identifier => todo!(),
            TokenKind::Eof => todo!(),
            TokenKind::LBrace => todo!(),
            TokenKind::RBrace => todo!(),
            TokenKind::LParen => todo!(),
            TokenKind::RParen => todo!(),
            TokenKind::SemiColon => todo!(),
            TokenKind::Colon => todo!(),
            TokenKind::Comma => todo!(),
            TokenKind::Dot => todo!(),
            TokenKind::Comment => todo!(),
            TokenKind::DocComment => todo!(),
            TokenKind::Plus => todo!(),
            TokenKind::Minus => todo!(),
            TokenKind::Star => todo!(),
            TokenKind::Div => todo!(),
            TokenKind::Mod => todo!(),
            TokenKind::Increment => todo!(),
            TokenKind::Decrement => todo!(),
            TokenKind::Equal => todo!(),
            TokenKind::NotEqual => todo!(),
            TokenKind::Lesser => todo!(),
            TokenKind::Greater => todo!(),
            TokenKind::LesserEqual => todo!(),
            TokenKind::GreaterEqual => todo!(),
            TokenKind::Not => todo!(),
            TokenKind::Or => todo!(),
            TokenKind::And => todo!(),
            TokenKind::BitNot => todo!(),
            TokenKind::BitAnd => todo!(),
            TokenKind::BitOr => todo!(),
            TokenKind::BitXor => todo!(),
            TokenKind::LShift => todo!(),
            TokenKind::RShift => todo!(),
            TokenKind::Assign => todo!(),
            TokenKind::PlusAssign => todo!(),
            TokenKind::MinusAssign => todo!(),
            TokenKind::MulAssign => todo!(),
            TokenKind::DivAssign => todo!(),
            TokenKind::ModAssign => todo!(),
            TokenKind::BitAndAssign => todo!(),
            TokenKind::BitOrAssign => todo!(),
            TokenKind::BitXorAssign => todo!(),
            TokenKind::LShiftAssign => todo!(),
            TokenKind::RShiftAssign => todo!(),
            TokenKind::ThinArrow => todo!(),
        }
    }

    fn parse_type_decl(&self, tokens: &mut TokenStream) -> ParseOutput<AstNode> {
        let mut output = ParseOutput::<AstNode>::default();

        let type_keyword = output.merge(expect_token!(tokens, TokenKind::Type));

        let Some(type_name) = output.merge(self.parse_identifier(tokens)) else {
            return output;
        };

        let Some(type_generics) = output.merge(self.parse_generics_decl(tokens)) else {
            return output;
        };

        // if there is no assign token `=`, theres no way to keep parsing as its ambiguous what
        // would be next.
        let Some(_) = output.merge(expect_token!(tokens, TokenKind::Assign)) else {
            return output;
        };

        // if there is an invalid type kind after the assign token, there is also no way to keep
        // parsing.
        let Some((type_kind, ty_position)) = output.merge(expect_token_type_kind(tokens)) else {
            return output;
        };

        let Some(_) = output.merge(expect_token!(tokens, TokenKind::LBrace)) else {
            return output;
        };

        let value = match type_kind {
            TypeDeclKind::Struct => output.merge(self.parse_struct_decl(tokens, ty_position)),
            TypeDeclKind::Enum => todo!(),
        };

        let Some(value) = value else { return output };

        let Some(close_brace) = output.merge(expect_token!(tokens, TokenKind::RBrace)) else {
            return output;
        };

        if output.has_issues() {
            return output;
        }

        let keyword = type_keyword.expect("output had no issues, but failed to parse type keyword");
        let position = keyword.position.merge(close_brace.position);

        output.set_value(AstNode::TypeDecl(AstNodeTypeDecl {
            name: type_name,
            kind: type_kind,
            value: Box::new(value),
            generics: type_generics,
            position,
        }));

        output
    }

    fn parse_identifier(&self, tokens: &mut TokenStream) -> ParseOutput<AstNodeIdentifier> {
        let identifier = tokens.next_token();
        if !matches!(identifier.kind, TokenKind::Identifier) {
            return ParseOutput::with_issue(ParseIssue::new(
                "expected identifier",
                identifier.position,
            ));
        }

        ParseOutput::with_value(AstNodeIdentifier {
            position: identifier.position,
        })
    }

    fn parse_struct_decl(
        &self,
        tokens: &mut TokenStream,
        keyword_position: Span,
    ) -> ParseOutput<AstNode> {
        let mut output = ParseOutput::<AstNode>::default();

        let mut fields = vec![];

        while !matches!(tokens.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let field_name = output.merge(self.parse_identifier(tokens));
            let field_type = output.merge(self.parse_type_annotation(tokens));

            let next_token = tokens.peek_token();
            match next_token.kind {
                TokenKind::RBrace => {}
                TokenKind::Comma => tokens.consume(),
                TokenKind::Eof => output.add_issue(ParseIssue::new(
                    "unexpected end of file",
                    next_token.position,
                )),
                _ => output.add_issue(ParseIssue::new(
                    "struct fields must be separated by a comma",
                    next_token.position,
                )),
            }

            if output.has_issues() {
                return output;
            }

            let name = field_name.expect("output had no issues, but failed to parse field name");
            let ty = field_type.expect("output had no issues, but failed to parse field type");
            let position = name.position.merge(ty.position);

            fields.push(AstNodeStructField { name, ty, position });
        }

        if output.has_issues() {
            return output;
        }

        let position = keyword_position.merge(tokens.peek_token().position);
        output.set_value(AstNode::Struct(AstNodeStruct { fields, position }));

        output
    }

    fn parse_type_annotation(
        &self,
        tokens: &mut TokenStream,
    ) -> ParseOutput<AstNodeTypeAnnotation> {
        let mut output = ParseOutput::<AstNodeTypeAnnotation>::default();

        let Some(_) = output.merge(expect_token!(tokens, TokenKind::Colon)) else {
            return output;
        };

        if tokens.peek().is_primitive() {
            let type_node = self.parse_primitive_type(tokens);
            output.set_value(type_node);
            return output;
        }

        let Some(type_name) = output.merge(self.parse_identifier(tokens)) else {
            return output;
        };

        let Some(generics) = output.merge(self.parse_generics_decl(tokens)) else {
            return output;
        };

        let type_kind = AstNodeTypeKind::Named(AstNodeNamedType {
            name: type_name,
            position: type_name.position,
            generics,
        });

        let type_node = AstNodeTypeAnnotation {
            kind: type_kind,
            position: type_name.position,
        };

        output.set_value(type_node);

        output
    }

    fn parse_primitive_type(&self, tokens: &mut TokenStream) -> AstNodeTypeAnnotation {
        debug_assert!(tokens.peek().is_primitive());
        let token_primitive = tokens.next_token();

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

    fn parse_generics_decl(
        &self,
        tokens: &mut TokenStream,
    ) -> ParseOutput<Vec<AstNodeGenericDecl>> {
        let mut output = ParseOutput::<Vec<AstNodeGenericDecl>>::default();
        let mut generics = vec![];

        if !matches!(tokens.peek(), TokenKind::Lesser) {
            output.set_value(generics);
            return output;
        }

        let Some(_) = output.merge(expect_token!(tokens, TokenKind::Lesser)) else {
            return output;
        };

        while !matches!(tokens.peek(), TokenKind::Greater | TokenKind::Eof) {
            let Some(name) = output.merge(self.parse_identifier(tokens)) else {
                return output;
            };

            let next_token = tokens.peek_token();
            match next_token.kind {
                TokenKind::Greater => {}
                TokenKind::Comma => tokens.consume(),
                TokenKind::Eof => output.add_issue(ParseIssue::new(
                    "unexpected end of file",
                    next_token.position,
                )),
                _ => output.add_issue(ParseIssue::new(
                    "unclosed generic argument list",
                    next_token.position,
                )),
            }

            if output.has_issues() {
                return output;
            }

            generics.push(AstNodeGenericDecl {
                name,
                position: name.position,
            })
        }

        let Some(_) = output.merge(expect_token!(tokens, TokenKind::Greater)) else {
            return output;
        };

        if output.has_issues() {
            return output;
        }

        output.set_value(generics);
        output
    }
}

fn expect_token_type_kind(tokens: &mut TokenStream) -> ParseOutput<(TypeDeclKind, Span)> {
    let token_type_kind = tokens.next_token();

    if !matches!(token_type_kind.kind, TokenKind::Struct | TokenKind::Enum) {
        return ParseOutput::with_issue(ParseIssue::new(
            "invalid type kind, allowed type kinds would be `struct` or `enum` for example.",
            token_type_kind.position,
        ));
    }

    ParseOutput::with_value((token_type_kind.kind.into(), token_type_kind.position))
}
