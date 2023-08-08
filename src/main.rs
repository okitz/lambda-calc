extern crate lambda_calc;

use std::process;

fn main() {
    let input = "λ x . x";
    if let Err(e) = lambda_calc::run(input) {
        println!("Application error: {}", e);
        process::exit(1);
    }
}
