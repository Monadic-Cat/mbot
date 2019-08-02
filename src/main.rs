use std::env::args;
use mice::roll;

fn main() {
    let mut a = args();
    a.next();
    let input = a.fold(String::new(), |a, x| a + " " + &x);
    let result = roll(&input).unwrap();
    println!("{}", result);
}
