use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, BenchmarkGroup};
use ::rand::{RngCore, Rng};
use ::core::num::Wrapping;

/// Dice expressions we're using for benchmarks.
const DICE_EXPRESSIONS: &[(&str, &str, fn(&mut dyn RngCore) -> i64)] = &[
    ("Small Expression", "2d6", |rng| {
        (0..2).fold(Wrapping(0i64), |a, _| a + Wrapping(rng.gen_range(0, 6)) + Wrapping(1)).0
    }),
    ("Medium Expression", "2d6 + 4d4 + 9", |rng| {
        let die_2d6 = (0..2).fold(Wrapping(0i64), |a, _| a + Wrapping(rng.gen_range(0, 6)) + Wrapping(1));
        let die_4d4 = (0..4).fold(Wrapping(0i64), |a, _| a + Wrapping(rng.gen_range(0, 4)) + Wrapping(1));
        let nine = Wrapping(9);
        (die_2d6 + die_4d4 + nine).0
    }),
    ("Large Expression", "3d9 + 9d4 - 2d1 + 40d7", |rng| {
        let die_3d9 = (0..3).fold(Wrapping(0i64), |a, _| a + Wrapping(rng.gen_range(0, 9)) + Wrapping(1));
        let die_9d4 = (0..9).fold(Wrapping(0i64), |a, _| a + Wrapping(rng.gen_range(0, 4)) + Wrapping(1));
        let die_2d1 = Wrapping(2);
        let die_40d7 = (0..40).fold(Wrapping(0), |a, _| a + Wrapping(rng.gen_range(0, 7)) + Wrapping(1));
        (die_3d9 + die_9d4 - die_2d1 + die_40d7).0
    }),
    ("Absurdly Large Expression", "3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7", |_| 0),
    ("Expensive But Short Expression", "10000d100", |_| 0)
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
                let _ = black_box(::mice::parse::parse_expression(program_text.as_bytes()).unwrap());
            });
        });
        group.bench_function("New Stack Machine Compiler", |b| {
            b.iter(|| {
                let _ = black_box({
                    let (_, ast) = ::mice::parse::parse_expression(program_text.as_bytes()).unwrap();
                    ::mice::stack::compile(&ast)
                });
            });
        });
    };
    for (title, expression, _rust) in DICE_EXPRESSIONS {
        rollers(c.benchmark_group(format!("Startup - {}", title)), black_box(*expression));
    }
}

fn rolling_benchmark(c: &mut Criterion) {
    use ::rand::SeedableRng;
    let mut rng = ::rand::rngs::SmallRng::from_entropy();

    let mut rollers = |mut group: BenchmarkGroup<_>, program_text, rust: fn(&mut dyn RngCore) -> i64| {
        let expression = black_box(mice::parse::Expression::parse(program_text).unwrap().1.unwrap());
        let (_, program) = black_box(mice::parse::parse_expression(program_text.as_bytes()).unwrap());
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
        group.bench_function("Rust", |b| {
            b.iter(|| {
                let _ = black_box(rust(&mut rng));
            });
        });
    };

    let mut wg = |gn, t, rust| rollers(c.benchmark_group(gn), t, rust);
    for (title, expression, rust) in DICE_EXPRESSIONS {
        wg(format!("Execution - {}", title), expression, *rust);
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
                let (_, program) = mice::parse::parse_expression(program_text.as_bytes()).unwrap();
                black_box(mice::interp::interpret(&mut rng, &program))
            });
        });
        group.bench_function("New Stack Machine Roller", |b| {
            b.iter(|| {
                let (_, program) = mice::parse::parse_expression(program_text.as_bytes()).unwrap();
                let stack_program = mice::stack::compile(&program);
                let mut machine = ::mice::stack::Machine::new();
                let _ = black_box(machine.eval_with(&mut rng, &stack_program));
            });
        });
    };

    for (title, expression, _rust) in DICE_EXPRESSIONS {
        rollers(c.benchmark_group(format!("End to End - {}", title)), black_box(expression));
    }
}

criterion_group!(startup_benches, roll_startup_benchmark);
criterion_group!(benches, rolling_benchmark);
criterion_group!(end_to_end_benches, end_to_end_rolling_benchmark);
criterion_main!(startup_benches, benches, end_to_end_benches);
