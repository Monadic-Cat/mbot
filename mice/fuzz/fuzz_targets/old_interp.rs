#![no_main]
use libfuzzer_sys::fuzz_target;
use ::mice::parse::Expression;
use ::mice::util::ExpressionExt;
use ::mice::prelude::MiceFormat;
use ::rand_pcg::Pcg32;

const MAX_REPLY_LENGTH: usize = 1900;
fn format_smart(exp: mice::ExpressionResult) -> String {
    let first = exp.format(MiceFormat::new().total_right());
    let second;
    #[allow(clippy::blocks_in_if_conditions)]
    if first.len() < MAX_REPLY_LENGTH {
        first
    } else if {
        second = exp.format(MiceFormat::new().concise().total_right());
        second.len() < MAX_REPLY_LENGTH
    } {
        second
    } else {
        exp.total().to_string()
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(Ok((input, Ok(dice)))) = ::core::str::from_utf8(data).map(|data| Expression::parse(data)) {
        if input.is_empty() && !dice.exceeds_cap(10000) {
            let mut rng = Pcg32::new(0xcafef00dd15ea5e5, 0xa02bdbf7bb3c0a7);
            let _ = dice.roll_with(&mut rng).map(|res| format_smart(res));
        }
    }
});
