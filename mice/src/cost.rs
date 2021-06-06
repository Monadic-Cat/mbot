//! Approximate cost models for dice programs used in various contexts.
use crate::tree::for_;

/// Evaluation cost.
/// This is a context relative measure of how expensive it is to handle something.
/// In the interpreter contexts, this is most likely an approximation of
/// the count of the smallest significant step in interpretation evaluating given program
/// will be equivalent to.
/// In the formatting contexts, this is most likely a measure of how much allocation
/// will be performed.
/// In histogram plotting, this is simply a guessed at indicator of what multiple of the
/// fastest observed plotting time the generated data will take.
#[non_exhaustive]
pub enum Price {
    Bounded(u64),
    // Unbounded cost isn't a thing we need to handle yet,
    // but will be when we add exploding/bouncing dice.
    // Unbounded(u64),
}

impl ::core::ops::Add<Price> for Price {
    type Output = Price;
    fn add(self, rhs: Price) -> Self::Output {
        match (self, rhs) {
            (Price::Bounded(lhs), Price::Bounded(rhs)) => Price::Bounded(lhs.saturating_add(rhs)),
        }
    }
}
impl ::core::iter::Sum<Price> for Price {
    fn sum<I>(iter: I) -> Self
        where I: Iterator<Item = Price>,
    {
        // Since `Price` addition is saturating, we don't need to
        // worry about overflow checking here.
        iter.fold(Price::Bounded(0), |a, b| a + b)
    }
}

// Technically, this trait is open for use by types other than `Program`.
// This is intentional. I think it is quite likely I'll end up with a variety of
// types implementing this trait, where I need to demonstrate bounded cost to avoid being DOSed.
/// Cost of using a dice program in a given context.
pub trait Cost<'a, Ctx> {
    /// A runtime parameter for extra context information, should the `Ctx` type argument
    /// be insufficient for determining the cost of handling dice expressions
    /// in a given context.
    /// Where unimportant, this should be `()`.
    type Param;
    fn cost(&'a self, param: Self::Param) -> Price;
}
pub fn cost<'a, Ctx, T: Cost<'a, Ctx>>(thing: &'a T, param: T::Param) -> Price {
    <T as Cost<'a, Ctx>>::cost(thing, param)
}

use crate::parse::new::Program;
use crate::stack::postorder;

/// Context of AST walking interpreter.
pub struct AstInterp;
impl<'a> Cost<'a, AstInterp> for Program {
    type Param = ();
    fn cost(&'a self, _param: Self::Param) -> Price {
        use crate::parse::new::Term;
        let mut price = 0u64;
        postorder(self, |child, _parent| match child {
            Term::Constant(_) => price = price.saturating_add(1),
            Term::DiceRoll(count, sides) => {
                if *sides > 1 {
                    price = price.saturating_add(*count as u64);
                } else {
                    price = price.saturating_add(1);
                }
            }
            Term::KeepHigh(_, _) => price = price.saturating_add(1),
            Term::Add(_, _) | Term::Subtract(_, _) | Term::UnarySubtract(_) => {
                price = price.saturating_add(1)
            }
            Term::UnaryAdd(_) => (),
        });
        Price::Bounded(price)
    }
}

/// Context of stack bytecode interpreter.
pub struct StackInterp;
impl<'a> Cost<'a, StackInterp> for Program {
    type Param = ();
    fn cost(&'a self, _param: Self::Param) -> Price {
        use crate::parse::new::Term;
        let mut price = 0u64;
        postorder(self, |child, parent| match child {
            Term::Constant(_) => price = price.saturating_add(1),
            Term::DiceRoll(count, sides) => match parent {
                Some(Term::KeepHigh(_, keep_count)) => match (*keep_count == 0, *sides == 1) {
                    (true, _) => (),
                    (false, true) => price = price.saturating_add(1),
                    (false, false) => price += (*count as u64).saturating_mul(2),
                },
                _ => {
                    if *sides > 1 {
                        price = price.saturating_add(*count as u64);
                    } else {
                        price = price.saturating_add(1);
                    }
                }
            },
            Term::KeepHigh(_, _) => (),
            Term::Add(_, _) | Term::Subtract(_, _) | Term::UnarySubtract(_) => {
                price = price.saturating_add(1)
            }
            Term::UnaryAdd(_) => (),
        });
        Price::Bounded(price)
    }
}

// TODO: being able to implement this would allow us to remove the kludge inside
// ExpressionExt::evaluation_cost(...) having to do with the size of allocations in formatting.
/// Context of formatting the output of a dice program as text.
pub struct TextFormatOutput;
impl<'a> Cost<'a, TextFormatOutput> for crate::parse::Expression {
    // TODO: choose a better type for this?
    // FormatOptions is supposed to be deprecated, so using it in a new API is a little weird.
    type Param = &'a crate::post::FormatOptions;
    fn cost(&'a self, _param: Self::Param) -> Price {
        todo!()
    }
}

/// `mbot`(::mbot) centric cost calculations.
pub mod mbot {
    use ::core::marker::PhantomData;
    pub struct TextFormatOutput<T> { _priv: PhantomData<T> }
    pub struct Default;
    pub struct Short;
    pub struct Shortest;
    pub struct Combined;
}
impl<'a> Cost<'a, mbot::TextFormatOutput<mbot::Default>> for Program {
    type Param = ();
    fn cost(&'a self, _param: Self::Param) -> Price {
        use crate::parse::new::Term;
        let mut price = 0u64;
        // This doesn't need to be postorder- that's just what's most convenient.
        for_! { (term, _ancestors) in self.postorder() => {
            match term {
                Term::DiceRoll(count, _sides) => price = price.saturating_add(*count as u64),
                _ => price = price.saturating_add(1),
            }
        }}
        Price::Bounded(price)
    }
}
