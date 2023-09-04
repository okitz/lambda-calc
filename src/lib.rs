mod parser;
mod tokenizer;

use std::error::Error;

pub fn run(input: &str) -> Result<(), Box<dyn Error>> {
    let tstream = tokenizer::tokenize(input)?;
    println!("\nTokens: {:?}\n", tstream);
    let syntax_tree = parser::parse(tstream)?.unwrap();
    println!("str: {:?}", syntax_tree.root.borrow().stringify());

    syntax_tree.evaluate();
    println!("sub1: {:?}", syntax_tree.root.borrow().stringify());
    syntax_tree.evaluate();
    syntax_tree.evaluate();
    syntax_tree.evaluate();
    syntax_tree.evaluate();
    println!("sub2: {:?}", syntax_tree.root.borrow().stringify());

    Ok(())
}
