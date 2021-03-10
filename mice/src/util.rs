//! Nice to have utilities that aren't core to dice
//! manipulation itself, just handy for some reason.
#[cfg(feature = "thread_rng")]
use crate::parse::{Expression, Term};
use crate::{Error, ExpressionResult, parse::ParseError};
use thiserror::Error;

#[derive(Debug, Clone, Copy, Error)]
pub enum UtilError {
    #[error("tried to DOS me.")]
    ExceededCap,
    #[error("{0}")]
    RollError(Error),
}
impl From<Error> for UtilError {
    fn from(e: Error) -> Self {
        UtilError::RollError(e)
    }
}
impl From<ParseError> for UtilError {
    fn from(e: ParseError) -> Self {
        UtilError::RollError(e.into())
    }
}

type UResult = Result<ExpressionResult, UtilError>;

#[cfg(feature = "thread_rng")]
fn exceeds_cap(dice: &Expression, cap: i64) -> bool {
    evaluation_cost(dice, Some(cap)) > cap
}

fn evaluation_cost(dice: &Expression, cap: Option<i64>) -> i64 {
    let mut roll_count = 0;
    for term in dice.terms() {
        match term {
            // NOTE: this is a compromise on accuracy since
            // formatting the output of many d1s has greater than constant cost.          
            Term::Dice(d) => roll_count += d.number,
            // This branch only saves time
            // in the worst case - when there's
            // a truly obscene number of terms.
            Term::Constant(_) => roll_count += 1,
        }
        // Prevent worst case performance
        if let Some(cap) = cap {
            if roll_count > cap {
                return roll_count
            }
        }
    }
    roll_count
}

mod private {
    pub trait Sealed {}
    impl Sealed for crate::parse::Expression {}
}

/// An extension trait for `Expression`.
/// The idea here is to provide convenience methods
/// that could eventually be implemented solely in terms of public API.
///
/// This trait is sealed. It cannot be implemented outside this crate.
pub trait ExpressionExt: private::Sealed {
    /// Counts the number of evaluation steps
    /// it will take to compute a result of an expression,
    /// and returns whether that sum exceeds the given `cap`.
    ///
    /// Constant terms take one step, dice terms take one step for each die.
    /// An `Nd1` dice term counts as a constant term for the purpose of this sum.
    fn exceeds_cap(&self, cap: i64) -> bool;
    /// Counts the number of evaluation steps it will take to compute the result
    /// of an expression.
    ///
    /// Constant terms cost `1`, dice terms cost `1` per die, except `Nd1` dice terms,
    /// which costs `1`.
    ///
    /// A cap may be set, which will short circuit the cost calculation
    /// if the cap has been exceeded.
    fn evaluation_cost(&self, cap: Option<i64>) -> i64;
}
impl ExpressionExt for Expression {
    // Note that this method does not necessarily iterate over the whole expression.
    // In the case that the expression exceeds the given cap,
    // this method will return immediately upon discovering that.
    // Still, that does not make this suitable for dealing with truly absurd
    // sizes of untrusted input.
    // One might want integration with the parser for early termination, in such a case.
    // In my case, parsing and evaluating commands from Discord messages,
    // I know that the inputs I handle will not be large enough for this to matter.
    // (Due to Discord messages being limited to a maximum of 2k bytes.)
    // I would consider merging a PR that adds such functionality, if anyone is interested
    // in implementing it.
    fn exceeds_cap(&self, cap: i64) -> bool {
        exceeds_cap(self, cap)
    }
    fn evaluation_cost(&self, cap: Option<i64>) -> i64 {
        evaluation_cost(self, cap)
    }
}

#[cfg(feature = "thread_rng")]
#[derive(Error, Debug)]
#[error("tried to DOS me.")]
pub struct ExceededCap;

#[cfg(feature = "thread_rng")]
impl From<ExceededCap> for UtilError {
    fn from(_: ExceededCap) -> Self {
        Self::ExceededCap
    }
}

#[cfg(feature = "thread_rng")]
pub fn roll_exp_capped(dice: Expression, cap: i64) ->
    Result<Result<ExpressionResult, crate::Error>, ExceededCap>
{
    if !exceeds_cap(&dice, cap) {
        Ok(crate::roll_expr_iter_with(&mut rand::thread_rng(), dice.into_iter()))
    } else {
        Err(ExceededCap)
    }
}

/// For providing access to mice to irresponsible users
#[cfg(feature = "thread_rng")]
pub fn roll_capped(input: &str, cap: i64) -> UResult {
    let dice = crate::parse::wrap_dice(input)?;
    match roll_exp_capped(dice, cap) {
        Ok(Ok(x)) => Ok(x),
        Ok(Err(e)) => Err(e.into()),
        Err(e) => Err(e.into()),
    }
}
