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
use mice::roll;

fn main() {
    let mut a = args();
    a.next();
    let input = a.fold(String::new(), |a, x| a + " " + &x);
    let result = roll(&input).unwrap();
    println!("{}", result);
}
