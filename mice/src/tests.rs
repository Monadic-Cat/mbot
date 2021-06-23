fn ast_roll(expr: &str) -> (crate::parse::Program, crate::interp::ProgramOutput) {
    let (_input, (_tokens, program)) = crate::parse::parse_expression(expr.as_bytes()).unwrap();
    let output = crate::interp::interpret(&mut ::rand::thread_rng(), &program).unwrap();
    (program, output)
}
fn stack_roll(expr: &str) -> i64 {
    let (_input, (_tokens, program)) = crate::parse::parse_expression(expr.as_bytes()).unwrap();
    let stack_program = crate::stack::compile(&program);
    let mut machine = crate::stack::Machine::new();
    machine.eval_with(&mut ::rand::thread_rng(), &stack_program).unwrap()
}

#[test]
fn test_arithmetic() {
    fn assert_total(expr: &str, total: i64) {
        assert_eq!(ast_roll(expr).1.total(), total);
        assert_eq!(stack_roll(expr), total);
    }
    assert_total("2 + 2", 4);
    assert_total("2 - 2", 0);
    assert_total("5 - 3", 2);
}

fn test_expr(dice_expr: &str) -> Result<String, String> {
    let program = match crate::parse::parse_expression(dice_expr.as_bytes()) {
        Ok((_input, (_tokens, proggy))) => proggy,
        Err((_input, e)) => {
            return Err(format!("That's an invalid dice expression. {:?}", e));
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

#[test]
fn test_zero_sides() {
    println!("{}", test_expr("d0").unwrap_err());
}

#[test]
fn test_one_side() {
    println!("{}", test_expr("d1").unwrap());
}

#[test]
fn test_max_sides() {
    println!("{}", test_expr("d9223372036854775807").unwrap());
}


#[test]
fn test_keeping_highest_dice() {
    println!("{}", test_expr("4d6k3").unwrap());
}
