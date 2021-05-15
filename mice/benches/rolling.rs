use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, BenchmarkGroup};

/// Dice expressions we're using for benchmarks.
const DICE_EXPRESSIONS: &[(&str, &str)] = &[
    ("Small Expression", "2d6"),
    ("Medium Expression", "2d6 + 4d4 + 9"),
    ("Large Expression", "3d9 + 9d4 - 2d1 + 40d7"),
    ("Absurdly Large Expression", "3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7"),
    ("Expensive But Short Expression", "10000d100")
];

fn roll_startup_benchmark(c: &mut Criterion) {
    let rollers = |mut group: BenchmarkGroup<_>, program_text| {
        group.bench_function("Old Parser", |b| {
            b.iter(|| {
                let _ = black_box(::mice::parse::Expression::parse(program_text).unwrap().1.unwrap());
            });
        });
        group.bench_function("New AST Parser", |b| {
            b.iter(|| {
                let _ = black_box(::mice::parse::new::parse_expression(program_text.as_bytes()).unwrap());
            });
        });
        group.bench_function("New Stack Machine Compiler", |b| {
            b.iter(|| {
                let _ = black_box({
                    let (_, ast) = ::mice::parse::new::parse_expression(program_text.as_bytes()).unwrap();
                    ::mice::stack::compile(&ast)
                });
            });
        });
    };
    for (title, expression) in DICE_EXPRESSIONS {
        rollers(c.benchmark_group(format!("Startup - {}", title)), black_box(*expression));
    }
}

fn rolling_benchmark(c: &mut Criterion) {
    use ::rand::SeedableRng;
    let mut rng = ::rand::rngs::SmallRng::from_entropy();

    let mut rollers = |mut group: BenchmarkGroup<_>, program_text| {
        let expression = black_box(mice::parse::Expression::parse(program_text).unwrap().1.unwrap());
        let (_, program) = black_box(mice::parse::new::parse_expression(program_text.as_bytes()).unwrap());
        let stack_program = black_box(mice::stack::compile(&program));
        group.bench_function("Old Roller", |b| {
            b.iter(|| black_box(expression.roll_with(&mut rng)));
        });
        group.bench_function("New AST Roller", |b| {
            b.iter(|| black_box(mice::interp::interpret(&mut rng, &program)));
        });
        group.bench_function("New Stack Machine Roller", |b| {
            b.iter(|| {
                let mut machine = ::mice::stack::Machine::new();
                let _ = black_box(machine.eval_with(&mut rng, &stack_program));
            });
        });
    };

    let mut wg = |gn, t| rollers(c.benchmark_group(gn), t);
    for (title, expression) in DICE_EXPRESSIONS {
        wg(format!("Execution - {}", title), expression);
    }
}

fn end_to_end_rolling_benchmark(c: &mut Criterion) {
    use ::rand::SeedableRng;
    let mut rng = ::rand::rngs::SmallRng::from_entropy();
    let mut rollers = |mut group: BenchmarkGroup<_>, program_text: &str| {
        group.bench_function("Old Roller", |b| {
            b.iter(|| {
                let expression = mice::parse::Expression::parse(program_text).unwrap().1.unwrap();
                black_box(expression.roll_with(&mut rng))
            });
        });
        group.bench_function("New AST Roller", |b| {
            b.iter(|| {
                let (_, program) = mice::parse::new::parse_expression(program_text.as_bytes()).unwrap();
                black_box(mice::interp::interpret(&mut rng, &program))
            });
        });
        group.bench_function("New Stack Machine Roller", |b| {
            b.iter(|| {
                let (_, program) = mice::parse::new::parse_expression(program_text.as_bytes()).unwrap();
                let stack_program = mice::stack::compile(&program);
                let mut machine = ::mice::stack::Machine::new();
                let _ = black_box(machine.eval_with(&mut rng, &stack_program));
            });
        });
    };

    for (title, expression) in DICE_EXPRESSIONS {
        rollers(c.benchmark_group(format!("End to End - {}", title)), black_box(expression));
    }
}

criterion_group!(startup_benches, roll_startup_benchmark);
criterion_group!(benches, rolling_benchmark);
criterion_group!(end_to_end_benches, end_to_end_rolling_benchmark);
criterion_main!(startup_benches, benches, end_to_end_benches);
