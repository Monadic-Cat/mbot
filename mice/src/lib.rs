//! # mice, messing with dice
//!
//! This crate is written primarily for my own
//! usage, and will likely obtain extensions related
//! to games that I play.
#![allow(clippy::try_err)]
#[cfg(feature = "thread_rng")]
pub mod parse;
pub use parse::ParseError;
pub mod interp;
pub mod stack;
pub mod viz;
pub mod cost;
pub mod tree;

#[derive(::thiserror::Error, Debug, Clone, Copy)]
#[error("sum is too high for `i64`")]
pub struct OverflowPositive;
#[derive(::thiserror::Error, Debug, Clone, Copy)]
#[error("sum is too low for `i64`")]
pub struct OverflowNegative;
#[derive(::thiserror::Error, Debug, Clone, Copy)]
enum Overflow {
    #[error(transparent)]
    Positive(#[from] OverflowPositive),
    #[error(transparent)]
    Negative(#[from] OverflowNegative),
}

// TODO: seeded dice representation for reduced heap allocation?
// /// An evaluated dice term that avoids heap allocation for partial sums,
// /// in exchange for O(n) indexing, via iteration, of those partial sums.
// #[derive(Debug, Clone)]
// struct SeededDie {
//     total: i64,
//     sign_part: Sign,
//     sides: i64,
//     quantity: i64,
//     // TODO: decide on specific PRNG
//     rng: rand::rngs::StdRng,
// }
// impl SeededDie {
//     fn partial_sums(&self) -> impl Iterator<Item = i64> {
//         use ::core::iter;
//         let mut rng = self.rng.clone();
//         let sides = self.sides;
//         let quantity = self.quantity as usize;
//         iter::from_fn(move || Some(rng.gen_range(0, sides) + 1)).take(quantity)
//     }
// }
// fn seed_die_with<R: Rng>(a: &DiceTerm, rng: &mut R) -> Result<SeededDie, OverflowPositive> {
//     use ::rand::SeedableRng;
//     use ::rand::rngs::StdRng;
//     use ::core::iter;
//     let rng = StdRng::from_rng(rng).unwrap();
//     if a.count() > 1 {
//         let mut trng = rng.clone();
//         let mut terms = iter::from_fn(|| Some(trng.gen_range(0, a.sides()) + 1))
//             .take(a.count() as usize).map(|x| x as i64);
//         let total = terms.try_fold(0, i64::checked_add).ok_or(OverflowPositive)?;
//         Ok(SeededDie {
//             total,
//             rng,
//             sides: a.sides(),
//             quantity: a.count(),
//             sign_part: Sign::Positive,
//         })
//     } else {
//         Ok(SeededDie {
//             total: a.sides(),
//             sides: a.sides(),
//             sign_part: Sign::Positive,
//             quantity: 1,
//             rng,
//         })
//     }
// }


// N
// dN1   (+/-) N2
// N1dN2 (+/-) N3
// N1dN2 (+/-) N3dN4 (+/-) [...] (+/-) NN

#[cfg(test)]
mod tests {
    // /// A test of the determinism of `SeededDie`'s partial sums.
    // /// This test ensures that two invocations of `SeededDie::partial_sums`
    // /// will return iterators over identical contents.
    // #[test]
    // fn deterministic_partial_sums() {
    //     use super::seed_die_with;
    //     use super::DiceTerm;
    //     use ::rand::thread_rng;
    //     let term = DiceTerm {
    //         number: 10,
    //         size: 10,
    //     };
    //     let seeded = seed_die_with(&term, &mut thread_rng()).expect("10 * 10 < i64::MAX");
    //     let sums = seeded.partial_sums();
    //     let bsums = seeded.partial_sums();
    //     for (a, b) in sums.zip(bsums) {
    //         assert_eq!(a, b);
    //     }
    // }
    // /// A similar test of the determinism of `SeededDie`'s partial sums.
    // /// This test ensures that the precomputed total of a `SeededDie`'s
    // /// partial sums will be correct.
    // #[test]
    // fn correct_total() {
    //     use super::seed_die_with;
    //     use super::DiceTerm;
    //     use ::rand::thread_rng;
    //     let term = DiceTerm {
    //         number: 10,
    //         size: 10,
    //     };
    //     let seeded = seed_die_with(&term, &mut thread_rng()).expect("10 * 10 < i64::MAX");
    //     assert_eq!(seeded.total, seeded.partial_sums().sum::<i64>());
    // }
}
