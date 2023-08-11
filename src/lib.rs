mod parser;
mod tokenizer;

use std::error::Error;

pub fn run(input: &str) -> Result<(), Box<dyn Error>> {
    let tstream = tokenizer::tokenize(input)?;
    println!("\nTokens: {:?}\n", tstream);
    let syntax_tree = parser::parse(tstream)?;
    println!("AST: {:?}", syntax_tree.unwrap());
    Ok(())
}
