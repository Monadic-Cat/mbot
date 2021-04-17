use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, BenchmarkGroup};

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
    wg("Dice Rolling - Small Expression", "2d6");
    wg("Dice Rolling - Medium Expression", "2d6 + 4d4 + 9");
    wg("Dice Rolling - Large Expression", "3d9 + 9d4 - 2d1 + 40d7");
    wg("Dice Rolling - Absurdly Large Expression", "3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7 + 3d9 + 9d4 - 2d1 + 40d7");
    wg("Dice Rolling - Expensive But Short Expression", "10000d100");
}

fn compare_formatting(c: &mut Criterion) {
    let dice_result = black_box(mice::roll("2d6").unwrap());
    let format_cfg = mice::FormatOptions::new().total_right();
    let mut group = c.benchmark_group("FormatOptions");
    group.bench_function("Old Backend", |b| {
        b.iter(|| mice::nfmt::benching::old_format(&dice_result, format_cfg))
    });
    group.bench_function("Simple New Backend", |b| {
        b.iter(|| {
            mice::nfmt::format_compat(&dice_result, format_cfg);
        });
    });
    let mut buf = String::with_capacity(2000);
    group.bench_function("Simple New Backend - With Reusing", |b| {
        b.iter(|| {
            mice::nfmt::format_compat_with(&dice_result, &mut buf, format_cfg);
            black_box(&buf);
            buf.clear();
        });
    });
    group.finish();
}

criterion_group!(benches, rolling_benchmark);
criterion_group!(formatting_benches, compare_formatting);
criterion_main!(benches, formatting_benches);
