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

#[cfg_attr(test, derive(serde::Serialize, serde::Deserialize))]
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
    pub fn count(&self) -> i64 {
        self.number
    }
    pub fn sides(&self) -> i64 {
        self.size
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

#[cfg_attr(test, derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Copy, Clone)]
pub enum Term {
    Dice(DiceTerm),
    Constant(i64),
}
impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Term::Dice(x) => write!(f, "{}d{}", x.count(), x.sides()),
            Term::Constant(x) => write!(f, "{}", x),
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize, serde::Deserialize))]
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

mod hidden {
    #[cfg_attr(test, derive(serde::Serialize, serde::Deserialize))]
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

mod new {
    // A new parsing module, in my continuing effort to rework this entire thing.
    // Note that all byte strings in this module are conventionally UTF-8.
    // TODO: consider using the `bstr` crate
    use ::id_arena::{Arena, Id};
    use ::core_extensions::SliceExt;

    /// Operators
    // We could easily attach spans to these.
    #[derive(Debug, Copy, Clone)]
    enum Op {
        /// Addition
        Plus,
        /// Subtraction
        Minus,
    }

    /// A description of how tightly infix operators bind their arguments.
    /// This is how we handle precedence.
    fn infix_binding_power(op: Op) -> (u8, u8) {
        use Op::*;
        match op {
            Plus => (3, 4),
            Minus => (3, 4),
            // Multiplication might be 5 and 6
        }
    }

    /// Units of input as segmented by the lexer.
    #[derive(Debug)]
    enum Token {
        Int(u64),
        D,
        Op(Op),
        Whitespace,
    }

    /// Lex a dice expression. Returns a slice reference to trailing unlexed input.
    fn lex(input: &[u8]) -> (&[u8], Vec<Token>) {
        enum State<'a> {
            Normal,
            Int(&'a [u8]),
            // We don't care about whitespace, at least for now.
            // This just throws it out.
            Whitespace,
        }
        let mut tokens = Vec::with_capacity(input.len());
        let mut state = State::Normal;
        let mut cursor = input;
        loop {
            match state {
                State::Normal => match cursor {
                    [b'0'..=b'9', rest @ ..] => {
                        state = State::Int(cursor);
                        cursor = rest;
                    },
                    [b'd', rest @ ..] => {
                        tokens.push(Token::D);
                        cursor = rest;
                    },
                    [b'+', rest @ ..] => {
                        tokens.push(Token::Op(Op::Plus));
                        cursor = rest;
                    },
                    [b'-', rest @ ..] => {
                        tokens.push(Token::Op(Op::Minus));
                        cursor = rest;
                    },
                    [b'\t', rest @ ..] | [b' ', rest @ ..] => {
                        tokens.push(Token::Whitespace);
                        state = State::Whitespace;
                        cursor = rest;
                    },
                    [_, rest @ ..] => break,
                    [] => break,
                },
                State::Int(start) => match cursor {
                    [b'0'..=b'9', rest @ ..] => cursor = rest,
                    [..] => {
                        let slice = &start[..start.offset_of_slice(cursor)];
                        // TODO: handle too large
                        tokens.push(Token::Int(slice.iter().fold(0, |a, b| (a * 10) + (b - b'0') as u64)));
                        state = State::Normal;
                        // Note that we're specifically choosing not to advance the cursor here.
                    }
                },
                State::Whitespace => match cursor {
                    [b'\t', rest @ ..] | [b' ', rest @ ..] => cursor = rest,
                    [..] => state = State::Normal,
                }
            }
        }
        (cursor, tokens)
    }

    /// An AST node. This represents an expression as a tree of nodes stored in an [`Arena`].
    #[derive(Debug)]
    enum Term {
        Constant(u64),
        // This could conceivably have its arguments
        // replaced by terms, and be turned into an operator
        // in its own right. This could then allow strange expressions like `3d(d8)`.
        DiceRoll(u64, u64),
        Add(Id<Term>, Id<Term>),
        Subtract(Id<Term>, Id<Term>),
        UnarySubtract(Id<Term>),
        UnaryAdd(Id<Term>),
    }

    /// A parsed dice program. The result of invoking `parse_expression` on something like `"3d6 + 4"`.
    // Fuck it. Dice expressions are programs.
    pub struct Program {
        // Note that `Program`s are intended to be correctly formed by construction.

        // We allocate terms inside an arena so we can build trees without
        // allocating for each node.
        terms: Arena<Term>,
        top: Id<Term>,
    }

    /// For debugging purposes.
    /// This writes out the parse tree starting from `top` as an S-expression.
    fn write_sexpr(terms: &Arena<Term>, top: Id<Term>, buf: &mut String) {
        let mut write_op = |op: &str, lhs, rhs| {
            buf.push_str("(");
            buf.push_str(op);
            buf.push_str(" ");
            write_sexpr(terms, lhs, &mut *buf);
            buf.push_str(" ");
            write_sexpr(terms, rhs, &mut *buf);
            buf.push_str(")");
        };
        match terms[top] {
            Term::Constant(n) => { itoa::fmt(&mut *buf, n).unwrap(); },
            Term::DiceRoll(count, faces) => {
                itoa::fmt(&mut *buf, count).unwrap();
                buf.push_str("d");
                itoa::fmt(&mut *buf, faces).unwrap();
            }
            Term::Add(lhs, rhs) => write_op("+", lhs, rhs),
            Term::Subtract(lhs, rhs) => write_op("-", lhs, rhs),
            Term::UnaryAdd(arg) => {
                buf.push_str("+");
                write_sexpr(terms, arg, &mut *buf);
            }
            Term::UnarySubtract(arg) => {
                buf.push_str("-");
                write_sexpr(terms, arg, &mut *buf);
            }
        }
    }

    impl Program {
        /// For debugging purposes.
        /// This writes out the parse tree of a program as an S-expression.
        pub fn fmt_sexpr(&self) -> String {
            let mut buf = String::new();
            write_sexpr(&self.terms, self.top, &mut buf);
            buf
        }
    }

    /// The return type of a parser function that returns trailing unparsed input
    /// on both success and failure.
    type ParseResult<I, O, E> = Result<(I, O), (I, E)>;

    /// Dice program parser combinator.
    /// Consumes input until it reaches unrecognizable tokens,
    /// and attempts to build a dice program from the consumed input.
    pub fn parse_expression(input: &[u8]) -> ParseResult<&[u8], Program, ()> {
        let mut terms = Arena::<Term>::new();
        let (rest, tokens) = dbg!(lex(input));
        // To be used where we already know to expect a unary op.
        fn consume_unary_op(terms: &mut Arena<Term>, op: Op, input: &[Token]) -> Result<Id<Term>, ()>  {
            match op {
                Op::Plus => {
                    let term = Term::UnaryAdd(consume_expr(&mut *terms, 3, input)?.1);
                    Ok(terms.alloc(term))
                },
                Op::Minus => {
                    let term = Term::UnarySubtract(consume_expr(&mut *terms, 3, input)?.1);
                    Ok(terms.alloc(term))
                }
            }
        }

        /// Scroll past whitespace.
        fn ignore_whitespace(input: &[Token]) -> &[Token] {
            let mut cursor = input;
            while let [Token::Whitespace, rest @ ..] = cursor {
                cursor = rest;
            }
            cursor
        }

        fn consume_expr<'a>(terms: &mut Arena<Term>, min_bp: u8, input: &'a [Token])
                        -> Result<(&'a [Token], Id<Term>), ()> {
            let (mut cursor, mut lhs) = match ignore_whitespace(input) {
                // Currently we parse a dice term like a terminal, but
                // there is no reason we couldn't make `d` into an operator as well.
                // That said, dice terms are liable to become much more complicated.
                // For now, the extra flexibility that would come from that is
                // not necessary or wanted.
                [Token::Int(count), Token::D, Token::Int(faces), rest @ ..] => {
                    (rest, Term::DiceRoll(*count, *faces))
                },
                [Token::Int(n), rest @ ..] => (rest, Term::Constant(*n)),
                // TODO: return a more specific error here
                [x, ..] => todo!("handle invalid token in expression position: {:?}", x),
                [] => todo!("handle eof in expression position"),
            };

            loop {
                dbg!(&lhs);
                let (rest, op) = match cursor {
                    [Token::Op(op), rest @ ..] => (rest, op),
                    // TODO: return a more specific error here
                    [Token::Whitespace, rest @ ..] => {
                        cursor = rest;
                        continue
                    },
                    [x, ..] => todo!("handle invalid token in operator position: {:?}", x),
                    [] => break,
                };
                let (l_bp, r_bp) = infix_binding_power(*op);
                if l_bp < min_bp {
                    break
                }

                cursor = rest;
                let (rest, rhs) = dbg!(consume_expr(&mut *terms, r_bp, cursor)?);
                cursor = rest;
                match op {
                    Op::Plus => lhs = Term::Add(terms.alloc(lhs), rhs),
                    Op::Minus => lhs = Term::Subtract(terms.alloc(lhs), rhs),
                }
            }
            Ok((cursor, terms.alloc(lhs)))
        }

        let mut cursor = &tokens[..];
        let result = loop {
            match cursor {
                // Ignore preceding whitespace.
                [Token::Whitespace, rest @ ..] => cursor = rest,
                // Note that unary operations are currently only permitted at the
                // front of a dice expression. We could be more permissive than this,
                // but the current goal is identical behavior to the old parser.
                [Token::Op(op), rest @ ..] => break consume_unary_op(&mut terms, *op, rest),
                all @ [Token::Int(_), ..] |
                all @ [Token::D, ..] => break consume_expr(&mut terms, 2, all).map(|(_, x)| x),
                [] => todo!("handle unexpected end of valid input"),
            };
        };
        match dbg!(result) {
            Ok(top) => Ok((rest, Program { terms, top })),
            Err(()) => Err((rest, ())),
        }
    }
}
pub use new::parse_expression;
