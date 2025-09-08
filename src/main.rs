use loy_parser::ParseContext;
use miette::NamedSource;

fn main() -> miette::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let Some(filename) = args.get(1) else {
        eprintln!("USAGE: loy <path>");
        return Ok(());
    };

    let source = std::fs::read_to_string(filename).unwrap();
    let tokens = loy_lexer::Lexer::new(&source).lex().unwrap();

    let mut parse_context = ParseContext::new(tokens, &source);
    let ast = match loy_parser::parse_token_stream(&mut parse_context) {
        Ok(ast) => ast,
        Err(error) => Err(error
            .into_report()
            .with_source_code(NamedSource::new(filename, source.clone())))?,
    };

    println!("{}", ast.dump(&source));

    Ok(())
}
