//! # cmice, CLI mice
//! This is a command line dice expression evaluator.
//! Usage:
//! ```sh
//! cmice <dice expression>
//! ```
//! That's it.
//! The output format is unstable, because
//! [`mice`](https://crates.io/crates/mice) is unstable.
use std::env::args;

fn main() {
    let mut a = args();
    a.next();
    let input = a.fold(String::new(), |a, x| a + " " + &x);
    let program = match ::mice::parse::new::parse_expression(input.as_bytes()) {
        Ok((_input, (_tokens, proggy))) => proggy,
        Err(_) => {
            println!("That's an invalid dice expression.");
            ::std::process::exit(1)
        },
    };
    let result = ::mice::interp::interpret(&mut ::rand::thread_rng(), &program);
    match result {
        Ok(output) => println!("{}", ::mice::interp::fmt::mbot_format_default(program.terms(), &output)),
        Err(e) => eprintln!("{:?}", e),
    }
}
