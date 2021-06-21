

fn test_expr(dice_expr: &str) -> Result<String, String> {
    let program = match crate::parse::parse_expression(dice_expr.as_bytes()) {
        Ok((_input, (_tokens, proggy))) => proggy,
        Err((_input, e)) => {
            panic!("That's an invalid dice expression. {:?}", e);
        },
    };
    let result = crate::interp::interpret(&mut ::rand::thread_rng(), &program);
    match result {
        Ok(output) => Ok(crate::interp::fmt::mbot_format_default(program.terms(), &output)),
        Err(e) => return Err(format!("{:?}", e)),
    }
}

#[test]
fn test_basic_expressions() {
    println!("{}", test_expr("1d20+1d6+8").unwrap());
    println!("{}", test_expr("2d20+5").unwrap());
    println!("{}", test_expr("32d2").unwrap());
}

#[test]
fn test_leading_whitespace() {
    println!("{}", test_expr("                   1d20+1d6").unwrap());
}

#[test]
fn test_trailing_whitespace() {
    println!("{}", test_expr("1d20+1d6                     ").unwrap());
}

#[test]
fn test_mixed_whitespace() {
    println!("{}", test_expr("1d20       +         1d6").unwrap());
}

#[test]
fn test_maturity() {
    println!("{}", test_expr("69d420   +    13d37").unwrap());
}