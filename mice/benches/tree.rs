use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkGroup};
use ::mice::tree::for_;

// TODO: factor this out into a shared file
/// Dice expressions we're using for benchmarks.
const DICE_EXPRESSIONS: &[(&str, &str)] = &[
    ("Small Expression", "2d6"),
    ("Medium Expression", "2d6 + 4d4 + 9"),
    ("Large Expression", "3d9 + 9d4 - 2d1 + 40d7"),
    ("Absurdly Large Expression", "3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7"),
    ("Expensive But Short Expression", "10000d100")
];

fn postorder_iteration_benchmark(c: &mut Criterion) {
    let walkers = |mut group: BenchmarkGroup<_>, program| {
        group.bench_function("Recursive Walker", |b| {
            b.iter(|| {
                ::mice::stack::postorder(&program, |term, _parent| {
                    black_box(term);
                });
            });
        });
        group.bench_function("Explicit Stack Walker", |b| {
            b.iter(|| {
                for_! { (term, _ancestors) in program.postorder() => {
                    black_box(term);
                }}
            });
        });
    };
    for (title, expression) in DICE_EXPRESSIONS {
        walkers(c.benchmark_group(format!("Postorder Tree Iteration - {}", title)), black_box({
            ::mice::parse::parse_expression(expression.as_bytes()).unwrap().1.1
        }));
    }
}

fn stack_compiling_benchmark(c: &mut Criterion) {
    let walkers = |mut group: BenchmarkGroup<_>, program: ::mice::parse::Program| {
        pub struct StackProgram(Vec<Instruction>);
        #[derive(Copy, Clone)]
        enum Instruction {
            Value(i64),
            DiceRoll(i64, i64),
            // This currently gets a special instruction so we can maintain performance.
            DiceRollKeepHigh {
                count: i64,
                sides: i64,
                keep_count: i64,
            },
            Add,
            Subtract,
            UnarySubtract,
            NoOp,
        }
        group.bench_function("Recursive Walker", |b| {
            b.iter(|| {
                let mut instructions = Vec::with_capacity(program.terms().len());
                ::mice::stack::postorder(&program, |term, parent| {
                    use ::mice::parse::Term::*;
                    let next = match term {
                        Constant(value) => Instruction::Value(*value),
                        DiceRoll(count, sides) => match parent {
                            Some(KeepHigh(_, keep_count)) => Instruction::DiceRollKeepHigh {
                                count: *count,
                                sides: *sides,
                                keep_count: *keep_count,
                            },
                            _ => Instruction::DiceRoll(*count, *sides),
                        },
                        KeepHigh(_, _) => Instruction::NoOp,
                        Add(_, _) => Instruction::Add,
                        Subtract(_, _) => Instruction::Subtract,
                        // Unary addition is a no-op, so we compile it to one.
                        // If I were slightly less lazy, these would just not be pushed
                        // into the instruction listing.
                        UnaryAdd(_) => Instruction::NoOp,
                        UnarySubtract(_) => Instruction::UnarySubtract,
                        // Safety: I will ensure this is exhaustive for every update
                        // to the `Term` enum.
                        _ => unsafe { ::core::hint::unreachable_unchecked() },
                    };
                    instructions.push(next);
                });
                black_box(instructions);
            });
        });
        group.bench_function("Explicit Stack Walker", |b| {
            b.iter(|| {
                let mut instructions = Vec::with_capacity(program.terms().len());
                for_! { (term, mut ancestors) in program.postorder() => {
                    use ::mice::parse::Term::*;
                    let parent = ancestors.next();
                    let next = match term {
                        Constant(value) => Instruction::Value(*value),
                        DiceRoll(count, sides) => match parent {
                            Some(KeepHigh(_, keep_count)) => Instruction::DiceRollKeepHigh {
                                count: *count,
                                sides: *sides,
                                keep_count: *keep_count,
                            },
                            _ => Instruction::DiceRoll(*count, *sides),
                        },
                        KeepHigh(_, _) => Instruction::NoOp,
                        Add(_, _) => Instruction::Add,
                        Subtract(_, _) => Instruction::Subtract,
                        // Unary addition is a no-op, so we compile it to one.
                        // If I were slightly less lazy, these would just not be pushed
                        // into the instruction listing.
                        UnaryAdd(_) => Instruction::NoOp,
                        UnarySubtract(_) => Instruction::UnarySubtract,
                        // Safety: I will ensure this is exhaustive for every update
                        // to the `Term` enum.
                        _ => unsafe { ::core::hint::unreachable_unchecked() },
                    };
                    instructions.push(next);
                }}
                black_box(instructions);
            });
        });
    };
    for (title, expression) in DICE_EXPRESSIONS {
        walkers(c.benchmark_group(format!("Stack Compiling - {}", title)), black_box({
            ::mice::parse::parse_expression(expression.as_bytes()).unwrap().1.1
        }));
    }
}


criterion_group!(tree_iteration, postorder_iteration_benchmark);
criterion_group!(compiling, stack_compiling_benchmark);
criterion_main!(tree_iteration, compiling);
