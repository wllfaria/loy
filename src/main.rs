use miette::NamedSource;

fn main() -> miette::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let Some(filename) = args.get(1) else {
        eprintln!("USAGE: piller <path>");
        return Ok(());
    };

    let file_contents = std::fs::read_to_string(filename).unwrap();
    let tokens = piller_lexer::Lexer::new(&file_contents).lex().unwrap();

    match piller_parser::Parser::new().parse(tokens) {
        Ok(ast) => println!("{ast:#?}"),
        Err(error) => Err(error
            .into_report()
            .with_source_code(NamedSource::new(filename, file_contents)))?,
    };

    Ok(())
}