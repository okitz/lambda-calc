mod tokenizer;

use std::error::Error;

pub fn run(input: &str) -> Result<(), Box<dyn Error>> {
    let tstream = tokenizer::tokenize(input)?;
    println!("tstream: `{:?}`", tstream);
    Ok(())
}
