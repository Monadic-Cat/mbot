//! A brand new, even more overengineered, version of the formatting API.
//! Intended as a replacement for [`crate::FormatOptions`].
// I didn't much like the previous iteration of this crate's formatting facilities,
// so let's see if we can over engineer this in a *different* way!
use ::std::borrow::Cow;

/// A generic cons cell, used to represent heterogenous lists.
#[derive(Copy, Clone)]
pub struct Cons<Car, Cdr> {
    pub car: Car,
    pub cdr: Cdr,
}
/// The terminating type for [`Cons`] lists.
#[derive(Copy, Clone)]
pub struct Nil;

/// Not public API.
/// Constructs a non [`Nil`] terminated [`Cons`] list.
#[doc(hidden)]
#[macro_export]
macro_rules! tuple {
    () => { $crate::nfmt::Nil };
    ($only:expr) => { $only };
    ($first:expr , $($rest:expr),*) => {
        $crate::nfmt::Cons { car: $first, cdr: $crate::tuple! { $($rest),* } }
    }
}
/// Constructs a [`Nil`] terminated [`Cons`] list.
#[macro_export]
macro_rules! list {
    () => { Nil };
    ($only:expr) => { Cons { car: $only, cdr: $crate::nfmt::Nil } };
    ($first:expr , $($rest:expr),*) => {
        $crate::tuple!($first , $($rest),* , $crate::nfmt::Nil)
    }
}
macro_rules! tuple_ty {
    ($only:ty) => { $only };
    ($first:ty , $($rest:ty),*) => {
        Cons<$first, tuple_ty! { $($rest),* }>
    }
}
macro_rules! list_ty {
    () => { Nil };
    ($only:ty) => { Cons<$only, Nil> };
    ($first:ty , $($rest:ty),*) => {
        Cons<$first, list_ty! { $($rest),* }>
    }
}
#[macro_export]
macro_rules! tuple_pat {
    ($only:pat) => {
        $only
    };
    ($f:pat , $($p:pat),*) => {
        $crate::nfmt::Cons { car: $f, cdr: $crate::tuple_pat! { $($p),* } }
    }
}
#[macro_export]
macro_rules! list_pat {
    ($only:pat) => {
        $crate::nfmt::Cons { car: $only, cdr: $crate::nfmt::Nil }
    };
    ($f:pat , $($p:pat),*) => {
        $crate::nfmt::Cons { car: $f, cdr: $crate::list_pat! { $($p),* } }
    }
}

/// A description of how to format evaluated dice expressions.
/// The centerpiece of the new formatting API.
pub struct FormatDescriptor<Cns> {
    items: Cns,
}

/// Helper trait for unifying redundant type parameters.
pub trait Is<T> {}
impl<T> Is<T> for T {}
impl<Cns> FormatDescriptor<Cns> {
    pub const fn new<Car, Cdr>(items: Cons<Car, Cdr>) -> FormatDescriptor<Cons<Car, Cdr>>
    where Cons<Car, Cdr>: TopFormat,
    // This bound exists to help type inference. It changes no functionality.
    Cons<Car, Cdr>: Is<Cns>,
    {
        FormatDescriptor { items }
    }
    /// Format an evaluated expression with this [`FormatDescriptor`].
    pub fn format(&self, expr: &super::ExpressionResult) -> String
        where Cns: TopFormat,
    {
        let mut text = String::with_capacity(2000);
        self.items.fmt(expr, &mut text);
        text
    }
    /// Format an evaluated expression with this [`FormatDescriptor`],
    /// into an existing buffer.
    fn format_with(&self, expr: &super::ExpressionResult, out: &mut String)
        where Cns: TopFormat,
    {
        self.items.fmt(expr, out);
    }
}

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

pub struct Queryable {
    has_many: bool,
}
impl Queryable {
    fn has_many(&self) -> bool {
        self.has_many
    }
}

/// Trait for format items whose value is dependent on the length of an expression.
pub trait GlobalPropertyDependent<'a, Cdr, F> {
    fn of(&self, length: usize, query: Queryable) -> FormatItem<'a, Cdr, F>;
}
impl<'a, Cdr> GlobalPropertyDependent<'a, Cdr, ()> for () {
    fn of(&self, _length: usize, _query: Queryable) -> FormatItem<'a, Cdr, ()> {
        FormatItem::Nothing
    }
}
impl<'a, Cdr, F> GlobalPropertyDependent<'a, Cdr, ()> for F
where F: Fn(usize) -> FormatItem<'a, Cdr, ()> {
    fn of(&self, length: usize, _query: Queryable) -> FormatItem<'a, Cdr, ()> {
        self(length)
    }
}

/// A formatting instruction for [`FormatDescriptor`].
#[derive(Clone)]
#[non_exhaustive]
pub enum FormatItem<'a, Cdr, F = ()> {
    /// Insert the expression's total.
    Total,
    /// Insert the expression's terms.
    Terms(TermSeparator<'a>, TermFormatDescriptor<Cdr>),
    /// Insert arbitrary text.
    Text(Cow<'a, str>),
    /// Insert a formatting item based on the length of the expression.
    GlobalPropertyDependent(F),
    /// Insert nothing.
    Nothing,
    // TODO: a *Multiple* variant
}

/// Description of how to format a single evaluated term.
#[derive(Clone)]
pub struct TermFormatDescriptor<Cns> {
    items: Cns,
}
pub enum TermKind {
    Constant,
    Dice,
}
/// Trait for kind dependent term formatting instructions.
pub trait KindDependent<'a, F> {
    fn of(&self, kind: TermKind) -> TermFormatItem<'a, F>;
}
impl<'a> KindDependent<'a, ()> for () {
    fn of(&self, _kind: TermKind) -> TermFormatItem<'a, ()> {
        TermFormatItem::Nothing
    }
}
impl<'a, F> KindDependent<'a, ()> for F
where F: Fn(TermKind) -> TermFormatItem<'a, ()> {
    fn of(&self, kind: TermKind) -> TermFormatItem<'a, ()> {
        self(kind)
    }
}

#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub enum PartialSumSignDirective {
    Plus,
    Same,
}

/// A formatting instruction for [`TermFormatDescriptor`].
#[derive(Clone)]
#[non_exhaustive]
pub enum TermFormatItem<'a, F> {
    /// Insert the term's total.
    Total,
    /// Insert the term's partial sums.
    PartialSums(PartialSumSignDirective),
    /// Insert the term's expression.
    Expression,
    /// Insert the term's sign.
    Sign,
    /// Insert arbitrary text.
    Text(Cow<'a, str>),
    /// Insert a formatting item based on the kind of the term.
    KindDependent(F),
    /// Insert nothing.
    Nothing,
    // TODO: a *Multiple* variant
}

/// Trait for appending stuff to the end of a cons list.
pub trait ExtendList<List> {
    type Extended;
    fn extend(self, list: List) -> Self::Extended;
}
impl<List> ExtendList<List> for Nil {
    type Extended = List;
    fn extend(self, list: List) -> Self::Extended {
        list
    }
}
impl<Car, Cdr, List> ExtendList<List> for Cons<Car, Cdr>
    where Cdr: ExtendList<List>,
{
    type Extended = Cons<Car, Cdr::Extended>;
    fn extend(self, list: List) -> Self::Extended {
        let Cons { car, cdr } = self;
        let cdr = cdr.extend(list);
        Cons { car, cdr }
    }
}

fn sign_str(sign: crate::parse::Sign) -> &'static str {
    use crate::parse::Sign;
    match sign {
        Sign::Positive => "+",
        Sign::Negative => "-",
    }
}

/// A trait for formatting top level components of evaluated dice expressions.
pub trait TopFormat {
    fn fmt(&self, expr: &super::ExpressionResult, out: &mut String);
}
impl TopFormat for Nil {
    fn fmt(&self, _: &super::ExpressionResult, _: &mut String) {}
}

impl<'a, Cdr, F> TopFormat for FormatItem<'a, Cdr, F>
where F: GlobalPropertyDependent<'a, Cdr, ()>,
      Cdr: TermFormat
{
    fn fmt(&self, expr: &super::ExpressionResult, out: &mut String) {
        use FormatItem::*;
        use ::core::fmt::Write;
        match self {
            Total => itoa::fmt(out, expr.total()).unwrap(),
            Text(ref text) => out.push_str(text),
            GlobalPropertyDependent(ref func) => {
                let query = Queryable {
                    has_many: expr.pairs().len() > 1 && expr.pairs()[0].1.parts().len() > 1,
                };
                func.of(expr.pairs().len(), query).fmt(expr, out)
            },
            Terms(sep, terms) => {
                let mut pairs = expr.pairs().iter();
                if let Some(first) = pairs.next() {
                    terms.items.fmt(first, out);
                    for rest in pairs {
                        match sep {
                            TermSeparator::Comma => out.push_str(", "),
                            TermSeparator::Text(text) => out.push_str(text),
                            TermSeparator::Operator => {
                                out.push_str(" ");
                                out.push_str(sign_str(rest.0.sign));
                                out.push_str(" ");
                            },
                        }
                        terms.items.fmt(rest, out);
                    }
                }
            },
            Nothing => (),
        }
    }
}
impl<'top, Cdr, Cadr, F> TopFormat for Cons<FormatItem<'top, Cadr, F>, Cdr>
where Cdr: TopFormat,
      F: GlobalPropertyDependent<'top, Cadr, ()>,
      Cadr: TermFormat,
{
    fn fmt(&self, expr: &super::ExpressionResult, out: &mut String) {
        self.car.fmt(expr, out);
        self.cdr.fmt(expr, out);
    }
}

/// A trait for formatting individual terms of an evaluated dice expression.
pub trait TermFormat {
    fn fmt(&self, term: &(super::Expr, super::EvaluatedTerm), out: &mut String);
}
impl TermFormat for Nil {
    fn fmt(&self, _: &(super::Expr, super::EvaluatedTerm), _: &mut String) {}
}
impl<'a, F> TermFormat for TermFormatItem<'a, F>
where F: KindDependent<'a, ()>
{
    fn fmt(&self, term: &(super::Expr, super::EvaluatedTerm), out: &mut String) {
        use TermFormatItem::*;
        use ::core::fmt::Write;
        match self {
            Total => itoa::fmt(out, term.1.value()).unwrap(),
            PartialSums(directive) => {
                let mut parts = term.1.parts().iter();
                if let Some(first) = parts.next() {
                    itoa::fmt(&mut *out, *first).unwrap();
                    for rest in parts {
                        match directive {
                            PartialSumSignDirective::Plus => out.push_str(" +"),
                            PartialSumSignDirective::Same => {
                                out.push_str(" ");
                                out.push_str(sign_str(term.1.sign()));
                            },
                        }
                        out.push_str(" ");
                        itoa::fmt(&mut *out, *rest).unwrap();
                    }
                }
            },
            Expression => {
                use crate::parse::Term;
                match term.0.term {
                    Term::Dice(dice) => {
                        itoa::fmt(&mut *out, dice.count()).unwrap();
                        out.push_str("d");
                        itoa::fmt(out, dice.sides()).unwrap();
                    },
                    Term::Constant(number) => itoa::fmt(out, number).unwrap(),
                }
            },
            Sign => out.push_str(sign_str(term.0.sign)),
            Text(text) =>  out.push_str(text),
            KindDependent(func) => {
                let kind = match term.1 {
                    crate::EvaluatedTerm::Constant(_) => TermKind::Constant,
                    crate::EvaluatedTerm::Die(_) => TermKind::Dice,
                };
                func.of(kind).fmt(term, out);
            },
            Nothing => (),
        }
    }
}

impl<'a, Cdr, F> TermFormat for Cons<TermFormatItem<'a, F>, Cdr>
where Cdr: TermFormat,
      F: KindDependent<'a, ()>,
{
    fn fmt(&self, term: &(super::Expr, super::EvaluatedTerm), out: &mut String) {
        self.car.fmt(term, out);
        self.cdr.fmt(term, out);
    }
}

/// Format an evaluated expression using a [`FormatDescriptor`].
pub fn format<Cns>(expr: &super::ExpressionResult, desc: &FormatDescriptor<Cns>) -> String
where Cns: TopFormat,
{
    desc.format(expr)
}

/// Construct a [`Cons`] list of kind dependent [`TermFormatItem`]s.
#[macro_export]
macro_rules! depend_kind {
    (dice : [$($d_items:expr),+],
     constant: [$($c_items:expr),*]) => {
        $crate::list![
            $(
                $crate::nfmt::TermFormatItem::KindDependent(|kind| match kind {
                    $crate::nfmt::TermKind::Dice => $d_items,
                    $crate::nfmt::TermKind::Constant => $crate::nfmt::TermFormatItem::Nothing,
                })
            ),+ ,
            $(
                $crate::nfmt::TermFormatItem::KindDependent(|kind| match kind {
                    $crate::nfmt::TermKind::Dice => $crate::nfmt::TermFormatItem::Nothing,
                    $crate::nfmt::TermKind::Constant => $c_items,
                })
            ),*
        ]
    }
}

/// Construct a [`Cons`] list of length dependent [`FormatItem`]s.
#[macro_export]
macro_rules! depend_length {
    (ref |$len:ident| if $cond:expr => [$($t_items:expr),*]
     $(else [$($e_items:expr),*])?) => {
        $crate::list![
        $(
            $crate::nfmt::FormatItem::GlobalPropertyDependent(&(|$len| if $cond {
                $t_items
            } else {
                $crate::nfmt::FormatItem::Nothing
            }) as &dyn Fn(_) -> FormatItem<'static, Nil>)
        ),*
            $($(
                $crate::nfmt::FormatItem::GlobalPropertyDependent(&(|$len| if $cond {
                    $crate::nfmt::FormatItem::Nothing
                } else {
                    $e_items
                }) as &dyn Fn(_) -> FormatItem<'static, Nil>)
            ),*)?
        ]
    };
    (|$len:ident| if $cond:expr => [$($t_items:expr),*]
     $(else [$($e_items:expr),*])?) => {
        $crate::list![
        $(
            $crate::nfmt::FormatItem::GlobalPropertyDependent(|$len| if $cond {
                $t_items
            } else {
                $crate::nfmt::FormatItem::Nothing
            })
        ),*
            $($(
                $crate::nfmt::FormatItem::GlobalPropertyDependent(|$len| if $cond {
                    $crate::nfmt::FormatItem::Nothing
                } else {
                    $e_items
                })
            ),*)?
        ]
    }
}

struct IfMany<'a, Cdr, F> {
    item: FormatItem<'a, Cdr, F>,
}
impl<'a, Cdr, F> GlobalPropertyDependent<'a, Cdr, F> for IfMany<'a, Cdr, F>
where Cdr: Clone,
      F: Clone,
{
    fn of(&self, _length: usize, query: Queryable) -> FormatItem<'a, Cdr, F> {
        if query.has_many() {
            self.item.clone()
        } else {
            FormatItem::Nothing
        }
    }
}

// /// A trait for replacing items of [`Cons`] lists.
// trait ReplaceListItem<Pos, Item> {
//     type Replaced;
//     fn replace<N>(self, item: Item) -> Self::Replaced
//     // This bound exists only because we want to name
//     // the index at the call site, using turbofish syntax.
//     where N: Is<Pos>;
// }
// impl<Car, Cdr, Item> ReplaceListItem<typenum::U0, Item> for Cons<Car, Cdr> {
//     type Replaced = Cons<Item, Cdr>;
//     fn replace<N>(self, item: Item) -> Self::Replaced {
//         let Cons { car, cdr } = self;
//         let car = item;
//         Cons { car, cdr }
//     }
// }
// impl<Car, Cdr, Item, N> ReplaceListItem<N, Item> for Cons<Car, Cdr>
// where N: typenum::Unsigned + typenum::type_operators::IsGreater<typenum::U0>, {
    
// }

/// Incomplete.
///
/// A compatibility module for the old formatting facility.
/// This exists primarily to demonstrate that the new formatting facility
/// is at least as expressive as the old `FormatOptions` based one.
pub mod compat {
    use ::std::borrow::Cow;
    use super::{Cons, FormatDescriptor, Nil, FormatItem,
                TermSeparator, TermFormatDescriptor, TermFormatItem,
                PartialSumSignDirective, ExtendList, TermKind, IfMany};
    use crate::post::{ExpressionResult, FormatOptions, TotalPosition};
    pub(super) fn format_with(e: &ExpressionResult, options: FormatOptions, out: &mut String) {
        let default_term = depend_kind! {
            dice: [
                TermFormatItem::Text::<()>(Cow::Borrowed("(")),
                TermFormatItem::Expression::<()>,
                TermFormatItem::Text::<()>(Cow::Borrowed(" → ")),
                TermFormatItem::PartialSums::<()>(PartialSumSignDirective::Plus),
                TermFormatItem::Text::<()>(Cow::Borrowed(")"))
            ],
            constant: [
                TermFormatItem::Total::<()>
            ]
        };
        let default = list![
            FormatItem::Terms::<_, ()>(TermSeparator::Operator, TermFormatDescriptor {
                items: default_term.clone(),
            })];
        match options.total_position {
            TotalPosition::Left => {
                let left_total = depend_length!(|len| if len > 1 => [
                    FormatItem::Total::<Nil>,
                    FormatItem::Text::<Nil>(Cow::Borrowed(" = "))
                ]);
                if options.summarize_terms {
                    // TODO: write a trait to replace by index, to use instead of this
                    let tuple_pat![car, cdar, cdaar, _, cdr] = default_term;
                    let cdaaar = TermFormatItem::KindDependent(|kind| match kind {
                        TermKind::Dice => TermFormatItem::Total::<()>,
                        TermKind::Constant => TermFormatItem::Nothing
                    });
                    let term_fmt = tuple![car, cdar, cdaar, cdaaar, cdr];
                    let items = FormatItem::Terms::<_, ()>(TermSeparator::Operator, TermFormatDescriptor {
                        items: term_fmt,
                    });
                    let items = left_total.extend(items);
                    let descriptor = FormatDescriptor::new(items);
                    descriptor.format_with(e, out)
                } else {
                    let items = left_total.extend(default);
                    let descriptor = FormatDescriptor::new(items);
                    descriptor.format_with(e, out)
                }
            },
            TotalPosition::Right => {
                let right_total = depend_length!(|len| if len > 1 => [
                    FormatItem::Text::<Nil>(Cow::Borrowed(" = ")),
                    FormatItem::Total::<Nil>
                ]);
                let items = default.extend(right_total);
                let descriptor = FormatDescriptor::new(items);
                descriptor.format_with(e, out)
            },
            TotalPosition::Suppressed => {
                let descriptor = FormatDescriptor::new(default);
                descriptor.format_with(e, out)
            }
        }
    }
    pub fn format(e: &ExpressionResult, options: FormatOptions) -> String {
        let mut out = String::with_capacity(2000);
        format_with(e, options, &mut out);
        out
    }
    #[cfg(test)]
    #[test]
    fn mbot_compatibility() {
        // This is the main formatting configuration
        // for mbot, and the one that it most matters we get right.
        let mbot_main_format = FormatOptions::new().total_right().concise();

        let result = crate::roll("4d6 + 3").unwrap();

        let old_output = crate::display::format(&result, mbot_main_format);
        let new_output = format(&result, mbot_main_format);

        assert_eq!(old_output, new_output);
    }
}

// Okay, I got a carried away with that.
// Let's do this for real now.
mod simple {
    use crate::ExpressionResult;
    use crate::post::EvaluatedTerm;
    use crate::parse::Expr;
    use super::PartialSumSignDirective;
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
        pub fn terms<F: Fn(TermFormatter)>(&mut self, func: F) -> &mut Self {
            let ExpressionFormatter { buf, expr } = self;
            for term in expr.pairs() {
                let formatter = TermFormatter { buf, term };
                func(formatter)
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
                            buf.push_str(super::sign_str(term.1.sign()));
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
            self.buf.push_str(super::sign_str(self.term.0.sign));
            self
        }
        /// Insert arbitrary text.
        pub fn text(&mut self, text: &str) -> &mut Self {
            self.buf.push_str(text);
            self
        }
        /// Insert things based on the kind of the term.
        pub fn for_kind<F: Fn(&mut TermFormatter)>(&mut self, func: F) -> &mut Self {
            func(self);
            self
        }
    }
    fn format_result<F: Fn(ExpressionFormatter)>(expr: &ExpressionResult, buf: &mut String, func: F) {
        let formatter = ExpressionFormatter { buf, expr };
        func(formatter)
    }
}

#[doc(hidden)]
pub mod benching {
    // Exposing whatever we need for benchmarks.
    pub use crate::display::format as old_format;
    pub fn format_with(e: &crate::ExpressionResult, options: crate::FormatOptions, out: &mut String) {
        crate::nfmt::compat::format_with(e, options, out);
    }
}
