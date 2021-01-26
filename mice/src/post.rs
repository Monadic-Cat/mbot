use crate::error::Error;
use crate::parse::{Expr, Sign};
use std::fmt::{Display, Formatter};
use std::ops::Neg;
use std::slice;
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

pub(crate) type EResult = Result<ExpressionResult, Error>;

/// The result of evaluating a dice expression.
#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[derive(Debug, Clone)]
pub struct ExpressionResult {
    /// Private field because `Expr`'s layout isn't final.
    pairs: Vec<(Expr, EvaluatedTerm)>,
    total: i64,
}
#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl ExpressionResult {
    /// Sum of all evaluated terms.
    pub fn total(&self) -> i64 {
        self.total
    }
    #[cfg(feature = "wasm")]
    pub fn display(&self) -> String {
        format!("{}", self)
    }
    pub(crate) fn pairs(&self) -> &Vec<(Expr, EvaluatedTerm)> {
        &self.pairs
    }
    // TODO: Bind *this* for wasm?
    /// Afford some control of the output to the user,
    /// by allowing the specification of recognized customizations.
    pub fn format(&self, options: FormatOptions) -> String {
        crate::display::format(self, options)
    }
}
impl ExpressionResult {
    pub(crate) fn new(pairs: Vec<(Expr, EvaluatedTerm)>, total: i64) -> Self {
        Self { pairs, total }
    }
}

impl Display for ExpressionResult {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format(FormatOptions::new()))
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum TotalPosition {
    Left,
    Right,
    Suppressed,
}
#[derive(Debug, Clone, Copy)]
pub(crate) enum TermSeparator {
    PlusSign,
    Comma,
}

/// Formatting options for dice expressions.
/// Necessary for stability of user facing output,
/// which is this library's primary concern.
///
/// Default formatting MAY change between patch releases.
/// New public options MUST cause a minor version increment.
/// Changes to the behavior of an existing public option MUST
/// cause a minor version increment.
///
/// To get back the old formatting scheme:
/// ```
/// # use mice::prelude::*;
/// let format = MiceFormat::new()
///     .total_left()
///     .term_commas()
///     .term_list_parens()
///     .concise();
/// println!("{}", roll("2d6 + 3")?.format(format));
/// # Ok::<(), MiceError>(())
/// ```
#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {
    pub(crate) ignore_sign: bool,
    pub(crate) total_position: TotalPosition,
    pub(crate) summarize_terms: bool,
    pub(crate) term_separators: TermSeparator,
    pub(crate) term_parentheses: bool,
    pub(crate) term_list_parentheses: bool,
}
#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl FormatOptions {
    /// Obtain a new `FormatOptions` object.
    pub fn new() -> FormatOptions {
        FormatOptions {
            ignore_sign: false,
            total_position: TotalPosition::Suppressed,
            summarize_terms: false,
            term_separators: TermSeparator::PlusSign,
            term_parentheses: true,
            term_list_parentheses: false,
        }
    }
    /// Crate internal API. Do not mark public.
    pub(crate) fn exclude_sign(mut self) -> Self {
        self.ignore_sign = true;
        self
    }
    /// The total of an expression will appear on the
    /// right hand side of it, ` = total`.
    pub fn total_right(mut self) -> Self {
        self.total_position = TotalPosition::Right;
        self
    }
    /// The total of an expression will appear on the
    /// left hand side of it, `total = `.
    pub fn total_left(mut self) -> Self {
        self.total_position = TotalPosition::Left;
        self
    }
    /// The total of an expression will not appear.
    /// This is the current default.
    pub fn no_total(mut self) -> Self {
        self.total_position = TotalPosition::Suppressed;
        self
    }
    /// Dice term representations will be `(Dice → Total)`.
    pub fn concise(mut self) -> Self {
        self.summarize_terms = true;
        self
    }
    /// Dice term representations will be `(Dice → Die1 + Die2 + ...)`.
    /// This is the current default.
    pub fn verbose(mut self) -> Self {
        self.summarize_terms = false;
        self
    }
    /// Separate term results with commas. `Term1, Term2, ...`
    pub fn term_commas(mut self) -> Self {
        self.term_separators = TermSeparator::Comma;
        self
    }
    /// Separate term results with plus signs. `Term1 + Term2 + ...`
    /// This is the current default.
    pub fn term_pluses(mut self) -> Self {
        self.term_separators = TermSeparator::PlusSign;
        self
    }
    /// Wrap each dice term result in parentheses.
    /// This is the current default.
    pub fn dice_parens(mut self) -> Self {
        self.term_parentheses = true;
        self
    }
    /// Don't wrap each dice term result in parentheses.
    pub fn no_term_parens(mut self) -> Self {
        self.term_parentheses = false;
        self
    }
    /// Wrap entire list of terms in parentheses.
    pub fn term_list_parens(mut self) -> Self {
        self.term_list_parentheses = true;
        self
    }
    /// Don't wrap entire list of terms in parentheses.
    /// This is the current default.
    pub fn no_term_list_parens(mut self) -> Self {
        self.term_list_parentheses = false;
        self
    }
}
impl Default for FormatOptions {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct RolledDie {
    pub(crate) total: i64,
    pub(crate) parts: Vec<i64>,
    pub(crate) sign_part: Sign,
}
impl Neg for RolledDie {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self {
            total: -self.total,
            sign_part: -self.sign_part,
            parts: self.parts,
        }
    }
}
impl RolledDie {
    fn format(&self, options: FormatOptions) -> String {
        let FormatOptions {
            summarize_terms,
            ignore_sign,
            ..
        } = options;
        if summarize_terms {
            format!("{}", self.total)
        } else if self.parts.len() > 1 {
            let mut iter = self.parts.iter();
            let first_sign = if !ignore_sign {
                match self.sign_part {
                    Sign::Positive => "",
                    Sign::Negative => "-",
                }
            } else {
                ""
            };
            let mut nstr = format!(
                "{}{}",
                if !ignore_sign { first_sign } else { "" },
                iter.next().unwrap()
            );
            let sign_part = if !ignore_sign {
                self.sign_part
            } else {
                Sign::Positive
            };
            for x in iter {
                nstr.push_str(&format!(" {} {}", sign_part, x))
            }
            // nstr.push_str(&format!(" = {}", self.total));
            nstr
        } else {
            format!("{}", if ignore_sign { self.total.abs() } else { self.total })
        }
    }
}
impl Display for RolledDie {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format(FormatOptions::new()))
    }
}

#[derive(Debug, Clone)]
pub(crate) enum EvaluatedTerm {
    Die(RolledDie),
    Constant(i64),
}
fn format_i64(s: i64, options: FormatOptions) -> String {
    let FormatOptions { ignore_sign, .. } = options;
    if ignore_sign {
        match s.checked_abs() {
            Some(x) => format!("{}", x),
            // Unnecessarily expensive, but I'm not seeing
            // an API for this in `std`.
            None => String::from(&format!("{}", s)[1..]),
        }
    } else {
        format!("{}", s)
    }
}
impl EvaluatedTerm {
    pub(crate) fn value(&self) -> i64 {
        match self {
            EvaluatedTerm::Die(x) => x.total,
            EvaluatedTerm::Constant(x) => *x,
        }
    }
    pub(crate) fn parts(&self) -> &[i64] {
        match self {
            EvaluatedTerm::Die(x) => &x.parts,
            EvaluatedTerm::Constant(x) => slice::from_ref(x),
        }
    }
    pub(crate) fn sign(&self) -> Sign {
        match self {
            EvaluatedTerm::Die(x) => x.sign_part,
            EvaluatedTerm::Constant(x) => {
                if *x >= 0 {
                    Sign::Positive
                } else {
                    Sign::Negative
                }
            }
        }
    }
    pub(crate) fn format(&self, options: FormatOptions) -> String {
        match self {
            EvaluatedTerm::Die(x) => x.format(options),
            EvaluatedTerm::Constant(x) => format_i64(*x, options),
        }
    }
}
impl Neg for EvaluatedTerm {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            EvaluatedTerm::Die(x) => EvaluatedTerm::Die(-x),
            EvaluatedTerm::Constant(x) => EvaluatedTerm::Constant(-x),
        }
    }
}
impl From<RolledDie> for EvaluatedTerm {
    fn from(d: RolledDie) -> EvaluatedTerm {
        EvaluatedTerm::Die(d)
    }
}
impl Display for EvaluatedTerm {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        // write!(f, "{}", self.value())
        match self {
            EvaluatedTerm::Die(x) => write!(f, "{}", x),
            EvaluatedTerm::Constant(x) => write!(f, "{}", x),
        }
    }
}
