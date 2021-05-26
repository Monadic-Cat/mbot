/// Utilities for exposing crate internals to users
/// in a safe and stable fashion.
use std::convert::TryFrom;

use crate::{
    builder::RollBuilder,
    error::Error,
    parse::{wrap_dice, DiceTerm, Expr, Sign, Term, ParseError, InvalidDie},
    post::EResult,
};
pub(crate) type ExprTuple = (i64, i64);

impl TryFrom<ExprTuple> for Expr {
    type Error = InvalidDie;
    fn try_from(tup: ExprTuple) -> Result<Self, InvalidDie> {
        let (mut n, s) = tup;
        let sign = if n < 0 {
            n = -n;
            Sign::Negative
        } else {
            Sign::Positive
        };
        Ok(Self {
            term: match s.cmp(&1) {
                ::core::cmp::Ordering::Less => Term::Dice(DiceTerm::new(n, s)?),
                ::core::cmp::Ordering::Equal => Term::Constant(n),
                ::core::cmp::Ordering::Greater => return Err(InvalidDie),
            },
            sign,
        })
    }
}
impl From<Expr> for ExprTuple {
    fn from(e: Expr) -> ExprTuple {
        let t = match e.term {
            Term::Dice(x) => (x.count(), x.sides()),
            Term::Constant(x) => (x, 1),
        };
        match e.sign {
            Sign::Positive => t,
            Sign::Negative => (-t.0, t.1),
        }
    }
}

/// Get a `Vec` of tuples of the form:
/// (number of dice, number of faces)
///
/// Constant terms are expressed in the form: (value, 1)
///
/// There is no guarantee of the order of terms.
///
/// The only possible error here is `Error::InvalidExpression`.
/// Other errors may be encountered in this function's complement:
/// `roll_tuples`.
pub fn tuple_vec(input: &str) -> Result<Vec<ExprTuple>, ParseError> {
    let e = wrap_dice(input)?;
    Ok(e.into_iter().map(|x| x.into()).collect())
}
/// Roll and sum a slice of tuples, in the form
/// provided by this function's complement: `tuple_vec`
#[cfg(feature = "thread_rng")]
pub fn roll_tuples(input: &[ExprTuple]) -> EResult {
    RollBuilder::new().with_tuples(input).map_err(|e| Error::from(ParseError::from(e)))?.into_roll().unwrap().roll()
}
