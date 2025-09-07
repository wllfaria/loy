use loy_parser::{AstFmt, ParseContext};
use miette::NamedSource;

fn main() -> miette::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let Some(filename) = args.get(1) else {
        eprintln!("USAGE: loy <path>");
        return Ok(());
    };

    let source = std::fs::read_to_string(filename).unwrap();
    let tokens = loy_lexer::Lexer::new(&source).lex().unwrap();

    let result = loy_parser::parse_token_stream(&mut ParseContext {
        source: &source,
        tokens,
    });

    match result {
        Ok(ast) => println!("{}", ast.dump(&source)),
        Err(error) => {
            let report = error
                .into_report()
                .with_source_code(NamedSource::new(filename, source));
            eprintln!("{report:?}");
        }
    }

    Ok(())
}
