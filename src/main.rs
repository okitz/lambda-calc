extern crate lambda_calc;

use std::process;

fn main() {
    let input = "((λz. t) (\\x.x) a) (y z)";
    if let Err(e) = lambda_calc::run(input) {
        println!("Application error: {}", e);
        process::exit(1);
    }
}
