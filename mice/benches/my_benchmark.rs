use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn rolling_benchmark(c: &mut Criterion) {
    c.bench_function("rolls", |b| b.iter(|| mice::roll(black_box("100000d100"))));
}

fn compare_formatting(c: &mut Criterion) {
    let dice_result = black_box(mice::roll("24d6 + 3").unwrap());
    let format_cfg = black_box(mice::FormatOptions::new().total_right());
    let mut group = c.benchmark_group("FormatOptions");
    group.bench_function("Old Backend", |b| b.iter(|| dice_result.format(format_cfg)));
    group.bench_function("New Backend", |b| b.iter(|| mice::nfmt::compat::format(&dice_result, format_cfg)));
    group.finish();
}

criterion_group!(benches, rolling_benchmark);
criterion_group!(formatting_benches, compare_formatting);
criterion_main!(benches, formatting_benches);
