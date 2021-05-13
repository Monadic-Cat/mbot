//! # mice, messing with dice
//!
//! This crate is written primarily for my own
//! usage, and will likely obtain extensions related
//! to games that I play.
//!
//! Some basic usage:
//!
//! ```
//! # use mice::{roll, Error};
//! println!("{}", roll("2d6 + 3")?);
//!
//! println!("{}", roll("2d6 + 3")?.total());
//!
//! let result = roll("2d6 + 3")?;
//! println!("{}\n{}", result, result.total());
//! # Ok::<(), Error>(())
//! ```
//!
//! The parser accepts an arbitrary number of terms in a dice expression.
//! ```
//! # use mice::{roll, Error};
//! println!("{}", roll("9d8 + 4d2 - 5 - 8d7")?);
//! # Ok::<(), Error>(())
//! ```
#![forbid(unsafe_code)]
use rand::Rng;
mod error;
pub use error::Error;
use error::MyResult;
mod post;
use post::{EResult, EvaluatedTerm, RolledDie};
pub use post::{ExpressionResult, FormatOptions};
mod expose;
#[cfg(feature = "thread_rng")]
pub use expose::roll_tuples;
pub use expose::tuple_vec;
pub mod parse;
use parse::{DiceTerm, Expr, Sign, Term};
pub use parse::ParseError;
pub mod builder;
use builder::RollBuilder;
mod display;
pub mod prelude;
pub mod util;
pub mod nfmt;
pub mod interp;
pub mod stack;
pub mod viz;
pub mod cost;
pub mod jit;

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
impl From<Overflow> for Error {
    fn from(o: Overflow) -> Self {
        match o {
            Overflow::Positive(x) => Self::OverflowPositive(x),
            Overflow::Negative(x) => Self::OverflowNegative(x),
        }
    }
}
impl ::core::ops::Neg for Overflow {
    type Output = Overflow;
    fn neg(self) -> Self::Output {
        use Overflow::*;
        match self {
            Positive(x) => Negative(-x),
            Negative(x) => Positive(-x),
        }
    }
}
fn roll_die_with<R>(a: &DiceTerm, rng: &mut R) -> Result<RolledDie, OverflowPositive>
where
    R: Rng,
{
    if a.sides() == 1 {
        Ok(RolledDie {
            total: a.count(),
            parts: (0..a.count()).map(|_| 1).collect(),
            sign_part: Sign::Positive,
        })
    } else {
        let mut total: i64 = 0;
        let mut parts = Vec::with_capacity(a.count() as usize);
        // Rng::gen_range has an exlusive upper bound
        for _ in 0..a.count() {
            let random = rng.gen_range(0, a.sides()) + 1;
            total = total.checked_add(random).ok_or(OverflowPositive)?;
            parts.push(random);
        }
        Ok(RolledDie {
            total,
            parts,
            sign_part: Sign::Positive,
        })
    }
}

/// An evaluated dice term that avoids heap allocation for partial sums,
/// in exchange for O(n) indexing, via iteration, of those partial sums.
#[derive(Debug, Clone)]
struct SeededDie {
    total: i64,
    sign_part: Sign,
    sides: i64,
    quantity: i64,
    // TODO: decide on specific PRNG
    rng: rand::rngs::StdRng,
}
impl SeededDie {
    fn partial_sums(&self) -> impl Iterator<Item = i64> {
        use ::core::iter;
        let mut rng = self.rng.clone();
        let sides = self.sides;
        let quantity = self.quantity as usize;
        iter::from_fn(move || Some(rng.gen_range(0, sides) + 1)).take(quantity)
    }
}
fn seed_die_with<R: Rng>(a: &DiceTerm, rng: &mut R) -> Result<SeededDie, OverflowPositive> {
    use ::rand::SeedableRng;
    use ::rand::rngs::StdRng;
    use ::core::iter;
    let rng = StdRng::from_rng(rng).unwrap();
    if a.count() > 1 {
        let mut trng = rng.clone();
        let mut terms = iter::from_fn(|| Some(trng.gen_range(0, a.sides()) + 1))
            .take(a.count() as usize).map(|x| x as i64);
        let total = terms.try_fold(0, i64::checked_add).ok_or(OverflowPositive)?;
        Ok(SeededDie {
            total,
            rng,
            sides: a.sides(),
            quantity: a.count(),
            sign_part: Sign::Positive,
        })
    } else {
        Ok(SeededDie {
            total: a.sides(),
            sides: a.sides(),
            sign_part: Sign::Positive,
            quantity: 1,
            rng,
        })
    }
}

fn eval_term_with<R>(a: &Expr, rng: &mut R) -> Result<EvaluatedTerm, Overflow>
where
    R: Rng,
{
    let t: MyResult<_, Overflow> = match a.term {
        Term::Dice(x) => roll_die_with(&x, rng).into(),
        Term::Constant(x) => MyResult::Ok(EvaluatedTerm::Constant(x)),
    };
    // No positive number can overflow via negation.
    // Since terms are purely positive, this will never overflow.
    (a.sign * t).into()
}

/// Evaluate a dice expression!
/// This function takes the usual dice expression format,
/// and allows an arbitrary number of terms.
/// ```
/// # use mice::roll;
/// # use mice::Error;
/// let dice_expression = "d20 + 5 - d2";
/// println!("{}", roll(dice_expression)?);
/// # Ok::<(), Error>(())
/// ```
///
/// An `Err` is returned in the following cases:
///   - A d0 is used
///   - The sum of all terms is too high
///   - The sum of all terms is too low
///   - Nonsense input
#[cfg(feature = "thread_rng")]
pub fn roll(input: &str) -> EResult {
    Ok(RollBuilder::new().parse(input)?.into_roll().unwrap().roll()?)
}

fn try_roll_expr_iter_with<I, R>(rng: &mut R, input: I) -> EResult
where
    I: Iterator<Item = Result<Expr, Error>>,
    R: Rng,
{
    // let mut rng = thread_rng(); // This doesn't work in WASM?
    let mut pairs = Vec::with_capacity(input.size_hint().0);
    let mut total: i64 = 0;
    for x in input {
        match x {
            Ok(x) => {
                let res = eval_term_with(&x, rng)?;
                let res_val = res.value();
                pairs.push((x, res));
                total = total.checked_add(res_val).ok_or(if res_val > 0 {
                    Error::OverflowPositive(OverflowPositive)
                } else {
                    Error::OverflowNegative(OverflowNegative)
                })?;
            }
            Err(x) => return Err(x),
        }
    }
    Ok(ExpressionResult::new(pairs, total))
}

fn roll_expr_iter_with<I, R>(rng: &mut R, input: I) -> EResult
where
    I: Iterator<Item = Expr>,
    R: Rng,
{
    try_roll_expr_iter_with(rng, input.map(Ok))
}

// N
// dN1   (+/-) N2
// N1dN2 (+/-) N3
// N1dN2 (+/-) N3dN4 (+/-) [...] (+/-) NN

#[cfg(test)]
mod tests {
    use crate::{roll, DiceTerm};
    #[test]
    fn arithmetic() {
        assert_eq!(roll("5 + 3").unwrap().total(), 8);
        assert_eq!(roll("5 - 3").unwrap().total(), 2);
    }
    #[test]
    fn dice() {
        match DiceTerm::new(0, 0) {
            Ok(_) => panic!(),
            Err(_) => (),
        }
    }
    /// A test of the determinism of `SeededDie`'s partial sums.
    /// This test ensures that two invocations of `SeededDie::partial_sums`
    /// will return iterators over identical contents.
    #[test]
    fn deterministic_partial_sums() {
        use super::seed_die_with;
        use super::DiceTerm;
        use ::rand::thread_rng;
        let term = DiceTerm {
            number: 10,
            size: 10,
        };
        let seeded = seed_die_with(&term, &mut thread_rng()).expect("10 * 10 < i64::MAX");
        let sums = seeded.partial_sums();
        let bsums = seeded.partial_sums();
        for (a, b) in sums.zip(bsums) {
            assert_eq!(a, b);
        }
    }
    /// A similar test of the determinism of `SeededDie`'s partial sums.
    /// This test ensures that the precomputed total of a `SeededDie`'s
    /// partial sums will be correct.
    #[test]
    fn correct_total() {
        use super::seed_die_with;
        use super::DiceTerm;
        use ::rand::thread_rng;
        let term = DiceTerm {
            number: 10,
            size: 10,
        };
        let seeded = seed_die_with(&term, &mut thread_rng()).expect("10 * 10 < i64::MAX");
        assert_eq!(seeded.total, seeded.partial_sums().sum::<i64>());
    }
}
