use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn rolling_benchmark(c: &mut Criterion) {
    c.bench_function("rolls", |b| b.iter(|| mice::roll(black_box("100000d100"))));
}

criterion_group!(benches, rolling_benchmark);
criterion_main!(benches);
