//! Interpreter for dice programs. The current design is a basic recursive AST walker.
//! Not intended to be fast.
use crate::parse::new::{Program, Term};
use ::id_arena::{Arena, Id};
use ::rand::Rng;

#[derive(Debug)]
pub enum InterpError {
    OverflowPositive,
    OverflowNegative,
}

#[derive(Debug)]
pub struct ProgramOutput {
    total: i64,
    outputs: Arena<TermOutput>,
    top: Id<TermOutput>,
}
impl ProgramOutput {
    pub fn total(&self) -> i64 {
        self.total
    }
    pub fn top(&self) -> &TermOutput {
        &self.outputs[self.top]
    }
    pub fn get(&self, id: Id<TermOutput>) -> &TermOutput {
        &self.outputs[id]
    }
}

pub fn interpret<R: Rng>(
    rng: &mut R,
    Program { terms, top }: &Program,
) -> Result<ProgramOutput, InterpError> {
    let mut outputs = Arena::new();
    let top = interpret_term(rng, &terms, &mut outputs, *top)?;
    let total = outputs[top].total();
    Ok(ProgramOutput {
        total, outputs, top
    })
}

#[non_exhaustive]
#[derive(Debug)]
pub enum TermOutput {
    Constant(i64),
    DiceRoll(i64, Option<Vec<i64>>),
    Add(i64, Id<TermOutput>, Id<TermOutput>),
    Subtract(i64, Id<TermOutput>, Id<TermOutput>),
    UnarySubtract(i64, Id<TermOutput>),
    UnaryAdd(i64, Id<TermOutput>),
}
impl TermOutput {
    fn total(&self) -> i64 {
        use TermOutput::*;
        *match self {
            Constant(total)
            | DiceRoll(total, ..)
            | Add(total, ..)
            | Subtract(total, ..)
            | UnarySubtract(total, ..)
            | UnaryAdd(total, ..) => total,
        }
    }
}

fn interpret_term<R: Rng>(
    rng: &mut R,
    terms: &Arena<Term>,
    term_outputs: &mut Arena<TermOutput>,
    term: Id<Term>,
) -> Result<Id<TermOutput>, InterpError> {
    match terms[term] {
        Term::Constant(total) => Ok(term_outputs.alloc(TermOutput::Constant(total))),
        Term::DiceRoll(count, sides) => {
            if sides == 1 {
                Ok(term_outputs.alloc(TermOutput::DiceRoll(count, None)))
            } else {
                let mut total: i64 = 0;
                let mut parts = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    let random = rng.gen_range(0, sides) + 1;
                    total = total
                        .checked_add(random)
                        .ok_or(InterpError::OverflowPositive)?;
                    parts.push(random);
                }
                Ok(term_outputs.alloc(TermOutput::DiceRoll(total, Some(parts))))
            }
        }
        Term::Add(left, right) => {
            let (total, left, right) = interpret_term(&mut *rng, terms, &mut *term_outputs, left)
                .and_then(|left| {
                interpret_term(&mut *rng, terms, &mut *term_outputs, right).and_then(|right| {
                    let (left_total, right_total) =
                        (term_outputs[left].total(), term_outputs[right].total());
                    left_total
                        .checked_add(right_total)
                        .ok_or_else(|| {
                            if left_total > 0 || right_total > 0 {
                                InterpError::OverflowPositive
                            } else {
                                InterpError::OverflowNegative
                            }
                        })
                        .and_then(|total| Ok((total, left, right)))
                })
            })?;
            Ok(term_outputs.alloc(TermOutput::Add(total, left, right)))
        }
        Term::Subtract(left, right) => {
            let left = interpret_term(&mut *rng, terms, &mut *term_outputs, left)?;
            let right = interpret_term(&mut *rng, terms, &mut *term_outputs, right)?;
            let (left_total, right_total) =
                (term_outputs[left].total(), term_outputs[right].total());
            Ok(term_outputs.alloc(TermOutput::Subtract(
                left_total.checked_add(right_total).ok_or_else(|| {
                    if left_total > 0 || right_total < 0 {
                        InterpError::OverflowPositive
                    } else {
                        InterpError::OverflowNegative
                    }
                })?,
                left,
                right,
            )))
        }
        Term::UnarySubtract(term) => {
            let term = interpret_term(&mut *rng, terms, &mut *term_outputs, term)?;
            let term_total = term_outputs[term].total();
            Ok(term_outputs.alloc(TermOutput::UnarySubtract(
                term_total
                    .checked_neg()
                    .ok_or(InterpError::OverflowNegative)?,
                term,
            )))
        }
        Term::UnaryAdd(term) => {
            let term = interpret_term(&mut *rng, terms, &mut *term_outputs, term)?;
            let term_total = term_outputs[term].total();
            Ok(term_outputs.alloc(TermOutput::UnaryAdd(term_total, term)))
        }
    }
}
