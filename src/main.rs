use std::env::args;
use mice::roll_dice;

fn main() {
    let mut a = args();
    a.next();
    let input = a.fold(String::new(), |a, x| a + " " + &x);
    let result = roll_dice(&input).unwrap();
    println!("{}", result);
}
