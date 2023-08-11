extern crate lambda_calc;

use std::process;

fn main() {
    let input = "λx.x (y z)";
    if let Err(e) = lambda_calc::run(input) {
        println!("Application error: {}", e);
        process::exit(1);
    }
}
