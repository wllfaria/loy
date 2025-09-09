fn main() -> miette::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let Some(filename) = args.get(1) else {
        eprintln!("USAGE: loy <path>");
        return Ok(());
    };

    let mut driver = loy_driver::Driver::new();
    driver.compile(vec![filename.into()])?;

    Ok(())
}