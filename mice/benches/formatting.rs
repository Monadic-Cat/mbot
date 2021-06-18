use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn formatting(c: &mut Criterion) {
    let program = ::mice::parse::parse_expression("2d6".as_bytes()).unwrap().1.1;
    let dice_output = ::mice::interp::interpret(&mut ::rand::thread_rng(), &program).unwrap();
    let dice_output = black_box(dice_output);
    let mut group = c.benchmark_group("Formatting");
    group.bench_function("mbot default", |b| {
        b.iter(|| black_box(::mice::interp::fmt::mbot_format_default(program.terms(), &dice_output)));
    });
    group.bench_function("mbot short", |b| {
        b.iter(|| black_box(::mice::interp::fmt::mbot_format_short(program.terms(), &dice_output)));
    });
    group.finish();
}

criterion_group!(formatting_benches, formatting);
criterion_main!(formatting_benches);
