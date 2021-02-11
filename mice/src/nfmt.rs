//! A brand new, better, version of the formatting API.
//! Intended as a replacement for [`crate::FormatOptions`].
use crate::ExpressionResult;
use crate::post::EvaluatedTerm;
use crate::parse::Expr;
use ::std::borrow::Cow;

#[derive(Clone)]
#[non_exhaustive]
pub enum TermSeparator<'a> {
    Operator,
    // Note that these two separator kinds are only acceptable
    // because the only available operators, as yet, are addition and
    // subtraction, which can both be expressed by signs instead.
    // If/when we add more operators, these will be discouraged.
    Comma,
    Text(Cow<'a, str>),
}
pub enum TermKind {
    Constant,
    Dice,
}

#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub enum PartialSumSignDirective {
    Plus,
    Same,
}

fn sign_str(sign: crate::parse::Sign) -> &'static str {
    use crate::parse::Sign;
    match sign {
        Sign::Positive => "+",
        Sign::Negative => "-",
    }
}

// Okay, I got a carried away with that.
// Let's do this for real now.
pub struct ExpressionFormatter<'a> {
    buf: &'a mut String,
    expr: &'a ExpressionResult,
}
pub struct TermFormatter<'a> {
    buf: &'a mut String,
    term: &'a (Expr, EvaluatedTerm),
}
impl ExpressionFormatter<'_> {
    /// Insert the expression's total.
    pub fn total(&mut self) -> &mut Self {
        // No, really, this isn't going to fail.
        // I'm not inserting panicking machinery for this.
        let _ = itoa::fmt(&mut self.buf, self.expr.total());
        self
    }
    /// Insert the expression's terms.
    pub fn terms<F: Fn(TermFormatter)>(&mut self, separator: TermSeparator, func: F) -> &mut Self {
        let ExpressionFormatter { buf, expr } = self;
        let mut pairs = expr.pairs().iter();
        if let Some(first) = pairs.next() {
            // TODO: examine the necessity of this special case
            if let TermSeparator::Operator = separator {
                use crate::parse::Sign;
                match first.0.sign {
                    Sign::Negative => buf.push_str("-"),
                    Sign::Positive => (),
                }
            }
            let formatter = TermFormatter { buf, term: first };
            func(formatter);
            for term in pairs {
                match separator {
                    TermSeparator::Comma => buf.push_str(", "),
                    TermSeparator::Text(ref text) => buf.push_str(text),
                    TermSeparator::Operator => {
                        buf.push_str(" ");
                        buf.push_str(sign_str(term.0.sign));
                        buf.push_str(" ");
                    },
                }
                let formatter = TermFormatter { buf, term };
                func(formatter)
            }
        }
        self
    }
    /// Insert arbitrary text.
    pub fn text(&mut self, text: &str) -> &mut Self {
        self.buf.push_str(text);
        self
    }
    // Barely a convenience function.
    // You'd need to use the evaluation cost stuff in `crate::util`,
    // which operates on the input expression, not the result.
    /// Convenience function for inserting stuff based on
    /// whether there's multiple partial sums in the expression.
    pub fn if_many<F: Fn(&mut ExpressionFormatter)>(&mut self, func: F) -> &mut Self {
        if self.expr.pairs().len() > 1 && self.expr.pairs()[0].1.parts().len() > 1 {
            func(self);
        }
        self
    }
    pub fn for_many<F: Fn(&mut ExpressionFormatter, bool)>(&mut self, func: F) -> &mut Self {
        func(self, self.expr.pairs().len() > 1 && self.expr.pairs()[0].1.parts().len() > 1);
        self
    }
}
impl TermFormatter<'_> {
    /// Insert the term's total.
    pub fn total(&mut self) -> &mut Self {
        let _ = itoa::fmt(&mut self.buf, self.term.1.value());
        self
    }
    /// Insert the term's partial sums.
    pub fn partial_sums(&mut self, directive: PartialSumSignDirective) -> &mut Self {
        let Self { buf, term } = self;
        let mut parts = term.1.parts().iter();
        if let Some(first) = parts.next() {
            let _ = itoa::fmt(&mut *buf, *first);
            for rest in parts {
                match directive {
                    PartialSumSignDirective::Plus => buf.push_str(" +"),
                    PartialSumSignDirective::Same => {
                        buf.push_str(" ");
                        buf.push_str(sign_str(term.1.sign()));
                    },
                }
                buf.push_str(" ");
                let _ = itoa::fmt(&mut *buf, *rest);
            }
        }
        self
    }
    /// Insert the term's expression.
    pub fn expression(&mut self) -> &mut Self {
        use crate::parse::Term;
        let Self { buf, term } = self;
        match term.0.term {
            Term::Dice(dice) => {
                let _ = itoa::fmt(&mut *buf, dice.count());
                buf.push_str("d");
                let _ = itoa::fmt(&mut *buf, dice.sides());
            },
            Term::Constant(number) => {
                let _ = itoa::fmt(&mut *buf, number);
            },
        }
        self
    }
    /// Insert the term's sign.
    pub fn sign(&mut self) -> &mut Self {
        self.buf.push_str(sign_str(self.term.0.sign));
        self
    }
    /// Insert arbitrary text.
    pub fn text(&mut self, text: &str) -> &mut Self {
        self.buf.push_str(text);
        self
    }
    /// Insert things based on the kind of the term.
    pub fn for_kind<F: Fn(&mut TermFormatter, TermKind)>(&mut self, func: F) -> &mut Self {
        match self.term.1 {
            EvaluatedTerm::Constant(_) => func(self, TermKind::Constant),
            EvaluatedTerm::Die(_) => func(self, TermKind::Dice),
        }
        self
    }
}
/// Format a dice expression.
pub fn format_result<F: Fn(ExpressionFormatter)>(expr: &ExpressionResult, buf: &mut String, func: F) {
    let formatter = ExpressionFormatter { buf, expr };
    func(formatter)
}
pub fn format_compat_with(expr: &ExpressionResult, buf: &mut String, options: crate::FormatOptions) {
    use crate::post::TotalPosition;
    format_result(expr, buf, |mut f| {
        let insert_terms = |f: &mut ExpressionFormatter| {
            f.terms(TermSeparator::Operator, |mut f| {
                f.for_kind(|f, kind| match kind {
                    TermKind::Dice => {
                        // TODO: finish this
                        if options.term_parentheses {
                            f.text("(");
                        }
                        f.expression().text(" → ");
                        if options.summarize_terms {
                            f.total();
                        } else {
                            f.partial_sums(PartialSumSignDirective::Plus);
                        }
                        if options.term_parentheses {
                            f.text(")");
                        }
                    },
                    TermKind::Constant => { f.total(); }
                });
            });
        };
        macro_rules! term_list_parens {
            ($formatter:expr , $options:expr , $inner:expr) => {
                if $options.term_list_parentheses {
                    $formatter.text("(");
                }
                $inner;
                if $options.term_list_parentheses {
                    $formatter.text(")");
                }
            }
        }
        match options.total_position {
            TotalPosition::Left => {
                f.total().if_many(|f| {
                    f.text(" = ");
                    term_list_parens!(f, options, {
                        insert_terms(f);
                    });
                });
            },
            TotalPosition::Right => {
                f.if_many(|f| {
                    term_list_parens!(f, options, {
                        insert_terms(f);
                    });
                    f.text(" = ");
                }).total();
            },
            TotalPosition::Suppressed => {
                f.for_many(|f, many| {
                    if many {
                        term_list_parens!(f, options, {
                            insert_terms(f);
                        });
                    } else {
                        f.total();
                    }
                });
            }
        }
    });
}
pub fn format_compat(expr: &ExpressionResult, options: crate::FormatOptions) -> String {
    let mut buf = String::new();
    format_compat_with(expr, &mut buf, options);
    buf
}
#[cfg(test)]
#[test]
fn old_compat() {
    let format_cfg = crate::FormatOptions::new().total_right();

    let result = crate::roll("-2d3 + 2").unwrap();

    let old_output =  crate::display::format(&result, format_cfg);
    let new_output = format_compat(&result, format_cfg);
    assert_eq!(old_output, new_output);
}

#[doc(hidden)]
pub mod benching {
    // Exposing whatever we need for benchmarks.
    pub use crate::display::format as old_format;
}
