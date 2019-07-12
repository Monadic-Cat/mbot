//! # mice, messing with dice
//! The heading obviates the need for a body.
//!
//! This crate is written primarily for my own
//! usage, and will likely obtain extensions related
//! to games that I play.
#![forbid(unsafe_code)]
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    combinator::opt,
    multi::many0,
    sequence::tuple,
    IResult,
};
use rand::{thread_rng, Rng};
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;
// use wasm_bindgen::prelude::*;

#[derive(Debug)]
pub enum RollError {
    /// This indicates the usage of a d0
    InvalidDie,
    /// The sum of terms is greater than what an `i64` can hold
    OverflowPositive,
    /// The sum of terms is lower than what an `i64` can hold
    OverflowNegative,
    /// The expression evaluated isn't a valid dice expression
    InvalidExpression,
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
        }
    }
}

impl Error for RollError {}

#[derive(Debug)]
struct Die {
    number: u64,
    size: u64,
}
impl Die {
    #[allow(dead_code)]
    fn new(number: u64, size: u64) -> Result<Die, RollError> {
        // u64 type constraint means
        // we don't need to check if number < 0
        // Forbid d0. d1 is weird, but it
        // has a correct interpretation.
        if size < 1 {
            Err(RollError::InvalidDie)
        } else {
            Ok(Die { number, size })
        }
    }
}

#[derive(Debug)]
enum Term {
    Die(Die),
    Constant(u64),
}

#[derive(Debug)]
enum Sign {
    Positive,
    Negative,
}

#[derive(Debug)]
struct Expr {
    term: Term,
    sign: Sign,
}

type Expression = Vec<Expr>;

fn roll_die_with<R>(a: Die, rng: &mut R) -> Result<u64, RollError>
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

fn eval_term_with<R>(a: Expr, rng: &mut R) -> Result<i64, RollError>
where
    R: Rng,
{
    let t = match a.term {
        Term::Die(x) => roll_die_with(x, rng),
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
            Ok(x) => Ok(x),
            Err(_) => Err(RollError::OverflowPositive),
        },
        Err(x) => Err(x),
    }
}

fn sum_result_iter<I>(a: I) -> Result<i64, RollError>
where
    I: Iterator<Item = Result<i64, RollError>>,
{
    a.fold(Ok(0), |a, t| {
        let t = match t {
            Ok(x) => x,
            Err(x) => return Err(x),
        };
        match a {
            Ok(x) => match x.checked_add(t) {
                Some(x) => Ok(x),
                None => {
                    if t > 0 {
                        Err(RollError::OverflowPositive)
                    } else {
                        Err(RollError::OverflowNegative)
                    }
                }
            },
            Err(x) => Err(x),
        }
    })
}

fn sum_terms(a: Vec<Expr>) -> Result<i64, RollError> {
    let mut rng = thread_rng();
    sum_result_iter(a.into_iter().map(|x| eval_term_with(x, &mut rng)))
}

fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}
fn integer(input: &str) -> IResult<&str, u64> {
    let (input, int) = take_while1(is_dec_digit)(input)?;
    // Pretend to be a 63 bit unsigned integer.
    let i = match int.parse::<i64>() {
        // The only error possible here is
        // integer overflow.
        // This should emit a nom Failure
        Err(_) => {
            return Err(nom::Err::<(&str, nom::error::ErrorKind)>::Failure((
                input,
                nom::error::ErrorKind::TooLarge,
            )))
        }
        Ok(x) => x as u64,
    };
    Ok((input, i))
}

fn die(input: &str) -> IResult<&str, Term> {
    // number of dice : [integer]
    // separator      : "d"
    // size of dice   : integer
    let (input, d) = tuple((opt(integer), tag("d"), integer))(input)?;
    Ok((
        input,
        Term::Die(Die {
            number: match d.0 {
                Some(x) => x,
                None => 1,
            },
            size: d.2,
        }),
    ))
}

fn addition(input: &str) -> IResult<&str, Sign> {
    let (input, _) = tag("+")(input)?;
    Ok((input, Sign::Positive))
}
fn subtraction(input: &str) -> IResult<&str, Sign> {
    let (input, _) = tag("-")(input)?;
    Ok((input, Sign::Negative))
}

fn operator(input: &str) -> IResult<&str, Sign> {
    alt((addition, subtraction))(input)
}

fn whitespace(input: &str) -> IResult<&str, &str> {
    alt((tag(" "), tag("\t")))(input)
}

fn separator(input: &str) -> IResult<&str, Sign> {
    let (input, t) = tuple((many0(whitespace), operator, many0(whitespace)))(input)?;
    Ok((input, t.1))
}

fn constant(input: &str) -> IResult<&str, Term> {
    let i = integer(input)?;
    Ok((i.0, Term::Constant(i.1)))
}

fn term(input: &str) -> IResult<&str, Term> {
    alt((die, constant))(input)
}

fn dice(input: &str) -> IResult<&str, Expression> {
    // [(+/-)] die ((+/-) die)*
    let (input, s) = tuple((opt(separator), term, many0(tuple((separator, term)))))(input)?;
    let mut expression = vec![Expr {
        term: s.1,
        sign: match s.0 {
            Some(x) => x,
            None => Sign::Positive,
        },
    }];
    for t in s.2 {
        expression.push(Expr {
            term: t.1,
            sign: t.0,
        });
    }
    Ok((input, expression))
}

/// Wrap up getting errors from parsing a dice expression.
fn wrap_dice(input: &str) -> Result<Expression, RollError> {
    let (input, e) = match dice(input.trim()) {
        Ok(x) => x,
        Err(_) => return Err(RollError::InvalidExpression),
    };
    // Prevent weirdness like "10dlol" => 10
    if !input.is_empty() {
        Err(RollError::InvalidExpression)
    } else {
        Ok(e)
    }
}

/// Evaluate a dice expression!
/// This function takes the usual dice expression format,
/// and allows an arbitrary number of terms.
/// ```
/// # use mice::roll_dice;
/// # use mice::RollError;
/// let dice_expression = "d20 + 5 - d2";
/// println!("{}", roll_dice(dice_expression)?);
/// # Ok::<(), RollError>(())
/// ```
///
/// An `Err` is returned in the following cases:
///   - A d0 is used
///   - The sum of all terms is too high
///   - The sum of all terms is too low
///   - Nonsense input
pub fn roll_dice(input: &str) -> Result<i64, RollError> {
    match wrap_dice(input) {
        Ok(x) => Ok(sum_terms(x)?),
        Err(x) => Err(x),
    }
}

/// Get an iterator of tuples of the form:
/// (number of dice, number of faces)
///
/// Constant terms are expressed in the form: (value, 1)
///
/// There is no guarantee of the order of terms.
///
/// The only possible error here is `RollError::InvalidExpression`.
/// Other errors may be encountered in this function's complement:
/// `roll_vec`.
pub fn dice_vec(input: &str) -> Result<Vec<(i64, u64)>, RollError> {
    let e = wrap_dice(input)?;
    Ok(e.into_iter()
        .map(|x| {
            let t = match x.term {
                Term::Die(x) => (x.number as i64, x.size),
                Term::Constant(x) => (x as i64, 1),
            };
            match x.sign {
                Sign::Positive => t,
                Sign::Negative => (-t.0, t.1),
            }
        })
        .collect())
}
fn roll_iter<'a, I>(input: I) -> Result<i64, RollError>
where
    I: Iterator<Item = &'a (i64, u64)>,
{
    sum_terms(
        input
            .map(|x| {
                let (mut n, s) = x;
                let sign = if n < 0 {
                    n = -n;
                    Sign::Negative
                } else {
                    Sign::Positive
                };
                Expr {
                    term: Term::Die(Die {
                        number: n as u64,
                        size: *s,
                    }),
                    sign,
                }
            })
            .collect(),
    )
}
pub fn roll_vec(input: &Vec<(i64, u64)>) -> Result<i64, RollError> {
    roll_iter(input.iter())
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
