//! # mice, messing with dice
//! The heading obviates the need for a body.
//!
//! This crate is written primarily for my own
//! usage, and will likely obtain extensions related
//! to games that I play.
#![forbid(unsafe_code)]
use rand::{thread_rng, Rng};
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;
// use wasm_bindgen::prelude::*;
mod parse;
use parse::{wrap_dice, Die, Expr, ParseError, Sign, Term};

pub(crate) type TResult = Result<i64, RollError>;

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Term::Die(x) => write!(f, "{}d{}", x.number, x.size),
            Term::Constant(x) => write!(f, "{}", x),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        // N
        // -N
        // NdN
        // -NdN
        let mut nstr = String::new();
        match self.sign {
            Sign::Positive => (),
            Sign::Negative => nstr.push_str("-"),
        }
        nstr.push_str(&format!("{}", self.term));
        write!(f, "{}", nstr)
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionResult {
    /// Private field because `Expr`'s layout isn't final.
    pairs: Vec<(Expr, i64)>,
    pub total: i64,
}
impl Display for ExpressionResult {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut nstr = self.total.to_string();
        if self.pairs.len() > 1 {
            nstr.push_str(" = (");
            let mut iter = self.pairs.iter();
            // Keep unwrap local so I can see *why* it's safe.
            // It will be easier to remove later if I change
            // the above.
            let first = iter.next().unwrap();
            nstr.push_str(&format!("{} -> {}", first.0, first.1));
            for x in iter {
                nstr.push_str(&format!(", {} -> {}", x.0, x.1));
            }
            nstr.push_str(")");
        }
        write!(f, "{}", nstr)
    }
}

/// Most general mice error type.
#[derive(Debug, Clone, Copy)]
pub enum RollError {
    /// This indicates the usage of a d0
    InvalidDie,
    /// The sum of terms is greater than what an `i64` can hold
    OverflowPositive,
    /// The sum of terms is lower than what an `i64` can hold
    OverflowNegative,
    /// The expression evaluated isn't a valid dice expression
    InvalidExpression,
    /// Miscellaneous error type. Only use in unstable APIs.
    MiscError { description: &'static str },
}
impl From<ParseError> for RollError {
    fn from(e: ParseError) -> Self {
        match e {
            ParseError::InvalidExpression => RollError::InvalidExpression,
        }
    }
}

impl Display for RollError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            RollError::InvalidDie => write!(f, "Invalid die"),
            RollError::OverflowPositive => write!(f, "sum is too high for `i64`"),
            RollError::OverflowNegative => write!(f, "sum is too low for `i64`"),
            RollError::InvalidExpression => {
                write!(f, "you've specified an invalid dice expression.")
            }
            RollError::MiscError { description: x } => write!(f, "{}", x),
        }
    }
}
impl Error for RollError {}
type EResult = Result<ExpressionResult, RollError>;

fn roll_die_with<R>(a: &Die, rng: &mut R) -> Result<u64, RollError>
where
    R: Rng,
{
    if a.size == 1 {
        Ok(a.number)
    } else if a.size < 1 {
        Err(RollError::InvalidDie)
    } else {
        let mut acc: u64 = 0;
        for n in (0..a.number).map(|_| rng.gen_range(1, a.size)) {
            acc = match acc.checked_add(n) {
                Some(x) => x,
                None => return Err(RollError::OverflowPositive),
            }
        }
        Ok(acc)
    }
}

fn eval_term_with<R>(a: &Expr, rng: &mut R) -> TResult
where
    R: Rng,
{
    let t = match a.term {
        Term::Die(x) => roll_die_with(&x, rng),
        Term::Constant(x) => Ok(x),
    };
    let p = match a.sign {
        Sign::Positive => match t {
            x => x,
        },
        Sign::Negative => match t {
            Ok(x) => Ok(x),
            Err(e) => match e {
                RollError::OverflowPositive => Err(RollError::OverflowNegative),
                x => Err(x),
            },
        },
    };
    match p {
        Ok(x) => match x.try_into() {
            Ok(x) => match a.sign {
                Sign::Positive => Ok(x),
                Sign::Negative => Ok(-x),
            },
            Err(_) => Err(RollError::OverflowPositive),
        },
        Err(x) => Err(x),
    }
}

/// Evaluate a dice expression!
/// This function takes the usual dice expression format,
/// and allows an arbitrary number of terms.
/// ```
/// # use mice::roll;
/// # use mice::RollError;
/// let dice_expression = "d20 + 5 - d2";
/// println!("{}", roll(dice_expression)?);
/// # Ok::<(), RollError>(())
/// ```
///
/// An `Err` is returned in the following cases:
///   - A d0 is used
///   - The sum of all terms is too high
///   - The sum of all terms is too low
///   - Nonsense input
pub fn roll(input: &str) -> EResult {
    match wrap_dice(input) {
        Ok(x) => Ok(roll_expr_iter(x.into_iter())?),
        Err(x) => Err(RollError::from(x)),
    }
}

type ExprTuple = (i64, u64);

/// Get a `Vec` of tuples of the form:
/// (number of dice, number of faces)
///
/// Constant terms are expressed in the form: (value, 1)
///
/// There is no guarantee of the order of terms.
///
/// The only possible error here is `RollError::InvalidExpression`.
/// Other errors may be encountered in this function's complement:
/// `roll_tupls`.
pub fn tupl_vec(input: &str) -> Result<Vec<ExprTuple>, RollError> {
    let e = wrap_dice(input)?;
    Ok(e.into_iter().map(|x| x.into()).collect())
}

impl From<ExprTuple> for Expr {
    fn from(tup: ExprTuple) -> Self {
        let (mut n, s) = tup;
        let sign = if n < 0 {
            n = -n;
            Sign::Negative
        } else {
            Sign::Positive
        };
        Self {
            term: Term::Die(Die {
                number: n as u64,
                size: s,
            }),
            sign,
        }
    }
}
impl From<Expr> for ExprTuple {
    fn from(e: Expr) -> ExprTuple {
        let t = match e.term {
            Term::Die(x) => (x.number as i64, x.size),
            Term::Constant(x) => (x as i64, 1),
        };
        match e.sign {
            Sign::Positive => t,
            Sign::Negative => (-t.0, t.1),
        }
    }
}

fn roll_expr_iter<I>(input: I) -> EResult
where
    I: Iterator<Item = Expr>,
{
    let mut rng = thread_rng();
    let mut pairs = Vec::new();
    let mut total: i64 = 0;
    for x in input {
        let res = match eval_term_with(&x, &mut rng) {
            Ok(x) => x,
            Err(x) => return Err(x),
        };
        pairs.push((x, res));
        match total.checked_add(res) {
            Some(x) => total = x,
            None => {
                return if res > 0 {
                    Err(RollError::OverflowPositive)
                } else {
                    Err(RollError::OverflowNegative)
                }
            }
        }
    }
    Ok(ExpressionResult { pairs, total })
}

fn roll_tupl_iter<'a, I>(input: I) -> EResult
where
    I: Iterator<Item = &'a ExprTuple>,
{
    let terms = input.map(|x| Expr::from(*x));
    roll_expr_iter(terms)
}
/// Roll and sum a slice of tuples, in the form
/// provided by this function's complement: `tupl_vec`
pub fn roll_tupls(input: &[ExprTuple]) -> EResult {
    roll_tupl_iter(input.iter())
}

// /// JavaScript binding for `roll_dice`.
// #[wasm_bindgen]
// pub fn roll(input: &str) -> Result<i64, JsValue> {
//     match roll_dice(input) {
//         Ok(x) => Ok(x),
//         Err(x) => Err(JsValue::from_str(&format!("{}", x))),
//     }
// }

// N
// dN1   (+/-) N2
// N1dN2 (+/-) N3
// N1dN2 (+/-) N3dN4 (+/-) [...] (+/-) NN

#[cfg(test)]
mod test {
    use crate::roll;
    #[test]
    fn arithmetic() {
        assert_eq!(roll("5 + 3").unwrap().total, 8);
        assert_eq!(roll("5 - 3").unwrap().total, 2);
    }
}
