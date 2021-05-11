//! Approximate cost models for dice programs used in various contexts.

/// Evaluation cost.
#[non_exhaustive]
pub enum Price {
    Bounded(u64),
    // Unbounded cost isn't a thing we need to handle yet,
    // but will be when we add exploding/bouncing dice.
    // Unbounded(u64),
}

/// Cost of using a dice program in a given context.
pub trait Cost<Ctx> {
    fn cost(&self) -> Price;
}

use crate::parse::new::Program;
use crate::stack::postorder;

/// Context of AST walking interpreter.
pub struct AstInterp;
impl Cost<AstInterp> for Program {
    fn cost(&self) -> Price {
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
impl Cost<StackInterp> for Program {
    fn cost(&self) -> Price {
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
