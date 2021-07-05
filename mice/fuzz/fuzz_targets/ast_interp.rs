#![no_main]
use libfuzzer_sys::fuzz_target;

use ::mice::parse::parse_expression;
use ::mice::interp::interpret;
use ::rand_pcg::Pcg32;

fuzz_target!(|data: &[u8]| {
    if let Ok((input, (_, program))) = parse_expression(data) {
        use ::mice::cost::{cost, AstInterp, Price};
        if !input.is_empty() { return }
        match cost::<AstInterp, _>(&program, ()) {
            // Dice programs permitted because cost estimation
            // machinery thinks they're cheap enough.
            Price::Bounded(price) if price <= 200 => {
                let mut rng = Pcg32::new(0xcafef00dd15ea5e5, 0xa02bdbf7bb3c0a7);
                let _ = interpret(&mut rng, &program);
            },
            // Dice programs refused for being too costly.
            _ => (),
        }
    }
});
