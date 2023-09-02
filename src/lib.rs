mod parser;
mod tokenizer;

use crate::parser::Tree;
use std::error::Error;

pub fn run(input: &str) -> Result<(), Box<dyn Error>> {
    let tstream = tokenizer::tokenize(input)?;
    println!("\nTokens: {:?}\n", tstream);
    let syntax_tree = parser::parse(tstream)?.unwrap();

    println!("str: {:?}", syntax_tree.stringify());
    println!(
        "leftmost: {:?}",
        Tree::find_leftmost_redex(&syntax_tree).unwrap().stringify()
    );
    Ok(())
}
