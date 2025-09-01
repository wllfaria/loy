fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<_>>();
    let Some(filename) = args.get(1) else {
        eprintln!("USAGE: piller <path>");
        return Ok(());
    };

    let file_contents = std::fs::read_to_string(filename)?;
    let mut lexer = piller_lexer::Lexer::new(&file_contents);
    let tokens = lexer.lex()?;

    println!("{tokens:#?}");

    Ok(())
}