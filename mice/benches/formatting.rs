use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, BenchmarkGroup};

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

criterion_group!(formatting_benches, compare_formatting);
criterion_main!(formatting_benches);