//! Types and parsers for dice expressions.
use crate::post::FormatOptions;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    combinator::opt,
    error::ErrorKind::TooLarge,
    multi::many0,
    sequence::tuple,
    Err::Failure,
    IResult,
};
use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::{Mul, Neg};
use thiserror::Error;
// use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Error)]
pub enum ParseError {
    #[error("you've specified an invalid dice expression")]
    InvalidExpression,
}

#[derive(Debug, Copy, Clone)]
pub struct DiceTerm {
    /// Negative numbers of dice are
    /// incorrect, but matching integer
    /// sizes is helpful.
    pub(crate) number: i64,
    /// Negative dice sizes are nonsense,
    /// but matching integer sizes are helpful.
    pub(crate) size: i64,
    // In particular, a proof we present in
    // `crate::eval_term_with` is only valid
    // due to our storing these things as
    // signed integer types,
    // despite their always being positive.
}
impl DiceTerm {
    /// Creation of a `Die` may fail if:
    ///  - number of sides < 1
    ///  - number of dice  < 0
    pub(crate) fn new(number: i64, size: i64) -> Result<Self, InvalidDie> {
        // Forbid d0 and below. d1 is weird, but it
        // has a correct interpretation.
        if size < 1 || number < 0 {
            Err(InvalidDie)
        } else {
            Ok(DiceTerm { number, size })
        }
    }
    pub fn count(&self) -> u64 {
        self.number as _
    }
    pub fn sides(&self) -> u64 {
        self.size as _
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct ConstantTerm {
    /// This is not allowed to exceed what would be the u63 max,
    /// and is not allowed to be less than zero.
    value: i64,
}
impl Display for ConstantTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Term {
    Dice(DiceTerm),
    Constant(i64),
}
impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Term::Dice(x) => write!(f, "{}d{}", x.number, x.size),
            Term::Constant(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Sign {
    Positive,
    Negative,
}
impl Neg for Sign {
    type Output = Sign;
    fn neg(self) -> Self::Output {
        match self {
            Sign::Positive => Sign::Negative,
            Sign::Negative => Sign::Positive,
        }
    }
}
impl<T: Neg<Output = T>> Mul<T> for Sign {
    type Output = T;
    fn mul(self, rhs: T) -> Self::Output {
        match self {
            Sign::Positive => rhs,
            Sign::Negative => -rhs,
        }
    }
}
impl Display for Sign {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Sign::Positive => "+",
                Sign::Negative => "-",
            }
        )
    }
}

mod hidden{
    #[derive(Debug, Copy, Clone)]
    pub struct Expr {
        pub(crate) term: super::Term,
        pub(crate) sign: super::Sign,
    }
}
pub(crate) use hidden::Expr;
impl Expr {
    pub(crate) fn format(&self, options: FormatOptions) -> String {
        // N
        // -N
        // NdN
        // -NdN
        let mut nstr = String::new();
        let FormatOptions { ignore_sign, .. } = options;
        if !ignore_sign {
            match self.sign {
                Sign::Positive => (),
                Sign::Negative => nstr.push_str("-"),
            }
        }
        nstr.push_str(&format!("{}", self.term));
        nstr
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format(FormatOptions::new()))
    }
}

#[derive(Debug)]
pub struct Expression {
    exprs: Vec<Expr>,
}
impl Expression {
    pub(crate) fn new(exprs: Vec<Expr>) -> Self {
        Expression { exprs }
    }
    pub(crate) fn iter(&self) -> ExpressionRefIterator<'_> {
        ExpressionRefIterator {
            internal_iterator: self.exprs.iter(),
        }
    }
    // This could be a trait implementation, but it's not supposed
    // to be visible outside of this crate.
    pub(crate) fn into_iter(self) -> ExpressionIterator {
        ExpressionIterator {
            internal_iterator: self.exprs.into_iter(),
        }
    }
    pub fn terms(&self) -> TermIter {
        TermIter { internal_iterator: self.iter() }
    }
    pub fn roll_with<R: ::rand::Rng>(&self, rng: &mut R) -> Result<crate::ExpressionResult, crate::Error> {
        crate::roll_expr_iter_with(rng, self.iter().copied())
    }
    #[cfg(feature = "thread_rng")]
    pub fn roll(&self) -> crate::EResult {
        self.roll_with(&mut ::rand::thread_rng())
    }
    /// Nom parser for an `Expression`.
    ///
    /// This is the same as `parse::dice`,
    /// provided here as well to help discoverability.
    pub fn parse(input: &str) -> IResult<&str, Result<Self, InvalidDie>> {
        dice(input)
    }
}
pub(crate) struct ExpressionRefIterator<'a> {
    internal_iterator: ::std::slice::Iter<'a, Expr>,
}
impl<'a> Iterator for ExpressionRefIterator<'a> {
    type Item = &'a Expr;
    fn next(&mut self) -> Option<Self::Item> {
        self.internal_iterator.next()
    }
}
pub(crate) struct ExpressionIterator {
    internal_iterator: ::std::vec::IntoIter<Expr>,
}
impl Iterator for ExpressionIterator {
    type Item = Expr;
    fn next(&mut self) -> Option<Self::Item> {
        self.internal_iterator.next()
    }
}
pub struct TermIter<'a> {
    internal_iterator: ExpressionRefIterator<'a>,
}
impl<'a> Iterator for TermIter<'a> {
    type Item = &'a Term;
    fn next(&mut self) -> Option<Self::Item> {
        self.internal_iterator.next().map(|x| &x.term)
    }
}

fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}

/// Parser for an effectively 63-bit unsigned integer.
///
/// This is useful because we represent signs separately
/// from the dice they might be seen as attached to.
/// This is due to a semantic difference.
/// e.g., In the real world, you don't have negative four dice.
/// You subtract the result of four dice.
pub fn integer(input: &str) -> IResult<&str, i64> {
    let (input, int) = take_while1(is_dec_digit)(input)?;
    // Pretend to be a 63 bit unsigned integer.
    let i = match int.parse::<i64>() {
        // The only error possible here is
        // integer overflow.
        // This should emit a nom Failure
        Err(_) => return Err(Failure((input, TooLarge))),
        Ok(x) => x,
    };
    Ok((input, i))
}

#[derive(Error, Debug)]
#[error("invalid die")]
pub struct InvalidDie;
impl From<InvalidDie> for ParseError {
    fn from(_: InvalidDie) -> Self {
        Self::InvalidExpression
    }
}

type PResult<I, O, PE, E = (I, ::nom::error::ErrorKind)> = Result<(I, Result<O, PE>), ::nom::Err<E>>;
fn okay<I, T, PE, E>(input: I, exp: T) -> Result<(I, Result<T, PE>), E> {
    Ok((input, Ok(exp)))
}
fn purr<I, T, PE, E>(input: I, err: PE) -> Result<(I, Result<T, PE>), E> {
    Ok((input, Err(err)))
}
macro_rules! trip {
    ($in:expr, $exp:expr) => {
        match $exp {
            Ok(x) => x,
            Err(e) => return Ok(($in, Err(e))),
        }
    }
}
fn die(input: &str) -> PResult<&str, DiceTerm, InvalidDie> {
    // number of dice : [integer]
    // separator      : "d"
    // size of dice   : integer
    let (input, (number, _, size)) = tuple((opt(integer), tag("d"), integer))(input)?;
    let number = number.unwrap_or(1);
    // Note that since we use the bare DiceTerm constructor,
    // we need to make certain no invalid dice are created.
    // That means `number` needs to be >= 0, and `size` needs to be >= 1.
    // Given that `integer` does not create integers less than zero,
    // the only check we need to do here is `size != 0`.
    if size != 0 {
        okay(input, DiceTerm { number, size })
    } else {
        purr(input, InvalidDie)
    }
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

/// Parser for a `+` or `-` sign.
pub fn sign(input: &str) -> IResult<&str, Sign> {
    // While currently equivalent to `operator`,
    // the idea of a sign will never change, whilst `operator`
    // may be extended to recognize other operators,
    // which would break someone who just wants
    // a positive or negative sign.
    // TL;DR: Arithmetic != Sign
    alt((addition, subtraction))(input)
}

/// Nom parser for whitespace
pub fn whitespace(input: &str) -> IResult<&str, &str> {
    alt((tag(" "), tag("\t")))(input)
}

fn separator(input: &str) -> IResult<&str, Sign> {
    tuple((many0(whitespace), operator, many0(whitespace)))(input).map(|(i, (_, op, _))| (i, op))
}

fn constant(input: &str) -> IResult<&str, ConstantTerm> {
    integer(input).map(|(i, int)| (i, ConstantTerm { value: int }))
}

// /// Use like this, where map is a HashMap: `|x| variable(map, x)`
// fn variable<'a>(dict: HashMap<&str, i64>, input: &'a str) -> IResult<&'a str, Term> {
//     let (input, id) = take_while1(|c: char| c.is_alphabetic())(input)?;
//     let v = dict.get(id)?;
//     Ok((input, Term::Constant(v)))
// }

fn term(input: &str) -> PResult<&str, Term, InvalidDie> {
    alt((
        |x| die(x).map(|(i, d)| (i, d.map(Term::Dice))),
        |x| constant(x).map(|(i, c)| (i, Ok(Term::Constant(c.value)))),
    ))(input)
}

/// Nom parser for a dice expression.
pub fn dice(input: &str) -> PResult<&str, Expression, InvalidDie> {
    // [(+/-)] dice ((+/-) dice)*
    let (input, (sign, term, terms)) =
        tuple((opt(separator), term, many0(tuple((separator, term)))))(input)?;
    let sign = sign.unwrap_or(Sign::Positive);
    let term = trip!(input, term);
    let mut expression = vec![Expr { term, sign }];
    for (sign, term) in terms {
        expression.push(Expr { term: trip!(input, term), sign })
    }
    okay(input, Expression::new(expression))
}

/// Wrap up getting errors from parsing a dice expression.
pub(crate) fn wrap_dice(input: &str) -> Result<Expression, ParseError> {
    let (input, e) = match dice(input.trim()) {
        Ok(x) => x,
        Err(_) => return Err(ParseError::InvalidExpression),
    };
    // Prevent weirdness like "10dlol" => 10
    if !input.is_empty() {
        Err(ParseError::InvalidExpression)
    } else {
        e.map_err(|e| e.into())
    }
}
