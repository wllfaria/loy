use loy_ast::token::{DisplaySource, Number, TokenKind};

use crate::lexer::Lexer;
use crate::result::Error;

#[test]
fn test_lexing_single_line_strings() {
    let first = "\"foo\"";
    let second = "\"foo \\\" bar\"";
    let source = [first, second].join("\n");

    let tokens = Lexer::new(&source)
        .lex()
        .unwrap()
        .into_iter()
        .collect::<Vec<_>>();

    assert_eq!(tokens[0].display_source(&source), "foo");
    assert_eq!(tokens[1].display_source(&source), "foo \\\" bar");
    assert!(tokens.len() == 3);
}

#[test]
fn test_lexing_unterminated_string() {
    let source = "\"foo bar baz\n\"";
    let result = Lexer::new(source).lex();
    assert_eq!(result.unwrap_err(), Error::UnterminatedString);

    let source = "\"foo bar baz";
    let result = Lexer::new(source).lex();
    assert_eq!(result.unwrap_err(), Error::UnexpectedEof);

    // escaping the newline makes this one valid.
    let source = "\"foo bar baz\\n\"";

    let tokens = Lexer::new(source)
        .lex()
        .unwrap()
        .into_iter()
        .collect::<Vec<_>>();
    assert_eq!(tokens[0].display_source(source), "foo bar baz\\n");
}

#[test]
fn test_lexing_shift_assign_operators() {
    let source = "<<= >>=";
    let tokens = Lexer::new(source)
        .lex()
        .unwrap()
        .into_iter()
        .collect::<Vec<_>>();

    assert_eq!(tokens[0].kind, TokenKind::LShiftAssign);
    assert_eq!(tokens[1].kind, TokenKind::RShiftAssign);
    assert!(tokens.len() == 3); // including EOF
}

#[test]
fn test_lexing_doc_comments() {
    let source = "/// This is a doc comment\n// This is a regular comment";
    let tokens = Lexer::new(source)
        .lex()
        .unwrap()
        .into_iter()
        .collect::<Vec<_>>();

    assert_eq!(tokens[0].kind, TokenKind::DocComment);
    assert_eq!(tokens[1].kind, TokenKind::Comment);
    assert!(tokens.len() == 3); // including EOF
}

#[test]
fn test_lexing_precedence_triple_vs_double() {
    let source = "<<= << >>= >>";
    let tokens = Lexer::new(source)
        .lex()
        .unwrap()
        .into_iter()
        .collect::<Vec<_>>();

    assert_eq!(tokens[0].kind, TokenKind::LShiftAssign);
    assert_eq!(tokens[2].kind, TokenKind::RShiftAssign);
    assert!(tokens.len() == 5); // including EOF
}

#[test]
fn test_lexing_comprehensive_example() {
    let source = "x <<= 5; /// doc comment";
    let tokens = Lexer::new(source)
        .lex()
        .unwrap()
        .into_iter()
        .collect::<Vec<_>>();

    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].kind, TokenKind::LShiftAssign);
    assert_eq!(tokens[2].kind, TokenKind::Number(Number::Unsigned(5)));
    assert_eq!(tokens[3].kind, TokenKind::SemiColon);
    assert_eq!(tokens[4].kind, TokenKind::DocComment);
    assert!(tokens.len() == 6); // including EOF
}