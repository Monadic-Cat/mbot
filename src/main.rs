//! # cmice, CLI mice
//! This is a command line dice expression evaluator.
//! Usage:
//! ```sh
//! cmice <dice expression>
//! ```
//! That's it.
//! The output format is unstable, because
//! [`mice`](https://crates.io/crates/mice) is unstable.
use mice::prelude::*;
use std::env::args;

fn main() {
    let default_format = MiceFormat::new().verbose().total_right();
    let mut a = args();
    a.next();
    let input = a.fold(String::new(), |a, x| a + " " + &x);
    let result = roll(&input).unwrap();
    println!("{}", result.format(default_format))
}
