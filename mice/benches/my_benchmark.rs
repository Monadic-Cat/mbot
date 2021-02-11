use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn rolling_benchmark(c: &mut Criterion) {
    c.bench_function("rolls", |b| b.iter(|| mice::roll(black_box("1000000d100"))));
}

fn compare_formatting(c: &mut Criterion) {
    let dice_result = black_box(mice::roll("1000000d100").unwrap());
    let format_cfg = mice::FormatOptions::new().total_right();
    let mut group = c.benchmark_group("FormatOptions");
    group.bench_function("Old Backend", |b| {
        b.iter(|| mice::nfmt::benching::old_format(&dice_result, format_cfg))
    });
    group.bench_function("New Backend", |b| {
        b.iter(|| mice::nfmt::compat::format(&dice_result, format_cfg))
    });
    let mut buf = String::with_capacity(200000);
    group.bench_function("New Backend - With Reusing", |b| {
        b.iter(|| {
            mice::nfmt::benching::format_with(&dice_result, format_cfg, &mut buf);
            buf.clear();
        });
    });
    let same_cfg_func = |mut f: ::mice::nfmt::simple::ExpressionFormatter| {
        f.if_many(|f| {
            f.terms(|mut f| {
                f.for_kind(|f, kind| {
                    use mice::nfmt::TermKind;
                    match kind {
                        TermKind::Dice => {
                            f.text("(").expression().text(" → ")
                                .partial_sums(mice::nfmt::PartialSumSignDirective::Plus)
                                .text(")");
                        },
                        TermKind::Constant => {
                            f.total();
                        }
                    }});
            }).text(" = ");
        }).total();
    };
    group.bench_function("Simple New Backend", |b| {
        b.iter(|| {
            let mut buf = String::with_capacity(2000);
            mice::nfmt::simple::format_result(&dice_result, &mut buf, same_cfg_func);
        });
    });
    let mut buf = String::with_capacity(2000);
    group.bench_function("Simple New Backend - With Reusing", |b| {
        b.iter(|| {
            mice::nfmt::simple::format_result(&dice_result, &mut buf, same_cfg_func);
            buf.clear();
        });
    });
    group.finish();
}

criterion_group!(benches, rolling_benchmark);
criterion_group!(formatting_benches, compare_formatting);
criterion_main!(benches, formatting_benches);
