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
        total,
        outputs,
        top,
    })
}

#[non_exhaustive]
#[derive(Debug)]
pub enum TermOutput {
    Constant(i64),
    DiceRoll(i64, Option<Vec<i64>>),
    KeepHigh {
        total: i64,
        keep_count: i64,
        roll: Id<TermOutput>,
    },
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
            | KeepHigh { total, .. }
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
        Term::KeepHigh(roll, count) => {
            let roll = interpret_term(rng, terms, term_outputs, roll)?;
            match &mut term_outputs[roll] {
                TermOutput::DiceRoll(total, partials) => match partials {
                    Some(partials) => {
                        use ::core_extensions::SliceExt;
                        partials.sort_unstable();
                        let total = partials.slice_lossy(0..(count as _), ()).iter().sum();
                        Ok(term_outputs.alloc(TermOutput::KeepHigh {
                            total,
                            // Note that the saved `keep_count` isn't guaranteed
                            // to match the actual number of partial sums.
                            // 4d6k5 will only keep 4 dice, but `keep_count` will be 5.
                            keep_count: count,
                            roll,
                        }))
                    }
                    None => if count >= 1 {
                        let total = *total;
                        Ok(term_outputs.alloc(TermOutput::KeepHigh {
                            total,
                            // Note that we preserve the behavior of the above branch,
                            // where `keep_count` is the same as requested,
                            // not what is actually received, here.
                            // d6k3 will only keep 1 die, but `keep_count` will be 3.
                            keep_count: count,
                            roll,
                        }))
                    } else {
                        Ok(term_outputs.alloc(TermOutput::KeepHigh {
                            total: 0,
                            keep_count: count,
                            roll,
                        }))
                    },
                },
                _ => unreachable!("nesting of dice operators is currently not permitted"),
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

// I'm having fun writing compatibility layers between things only I use. 🙃
/// A compatibility layer between [`ExpressionResult`](crate::ExpressionResult) and
/// [`ProgramOutput`]. Meant to allow code that uses [`nfmt`](crate::nfmt)
/// to switch to the new dice program stuff with minimal changes.
mod compat {
    use crate::parse::Expr;
    use crate::post::EvaluatedTerm;
    use crate::ExpressionResult;
    use super::ProgramOutput;
    use ::core::cell::RefCell;
    use ::once_cell::unsync::Lazy;
    trait Formatter {
        fn total(&mut self) -> &mut Self;
    }
}

/// A low level formatting API for dice program output.
mod fmt {
    
}

/// Just enough `nfmt` to let the inline roll commands formatting code work.
mod nfmt {
    use super::ProgramOutput;
    use ::core::mem::MaybeUninit;
    use ::core::pin::Pin;
    use fixed_str::Str;
    struct OverflowedCapacity<'p, 's> {
        unformatted: &'p [ProgramOutput],
        formatted: Vec<&'s str>,
    }
    // TODO: move stack allocated collection of strings to an appropriate crate
    struct Strings<const CAPACITY: usize, const IDX: usize> {
        buf: Str<CAPACITY>,
        index: [MaybeUninit<*const str>; IDX],
        count: usize,
    }
    impl<const C: usize, const I: usize> Strings<C, I> {
        fn new() -> Self {
            Self {
                buf: Str::zeroed(),
                index: [MaybeUninit::uninit(); I],
                count: 0,
            }
        }
        fn push(self: Pin<&mut Self>, text: &str) {
            todo!()
        }
        fn get(self: Pin<&Self>, idx: usize) -> Option<&str> {
            todo!()
        }
    }

    /// frick it, let's see how hard it is to format these off the bare AST anyway
    /// this takes a [`&mut Str<2000>`] because Discord's message length limit is 2000 UTF-8 bytes
    fn fmt_internal_rolls<'a, 'r>(rolls: &'r [ProgramOutput], buf: &'a mut Str<2000>)
                              -> Result<Vec<&'a str>, OverflowedCapacity<'r, 'a>> {
        let mut roll_ptrs = Vec::new();
        let mut cursor = buf.as_slice_mut();
        fn fmt_internal_roll<'a, 'r>(roll: &'r ProgramOutput, buf: &'a mut str)
                                 -> Result<(&'a mut str, &'a str), OverflowedCapacity<'r, 'a>> {
            todo!()
        }
        for roll in rolls {
            let (remaining_capacity, ptr) = fmt_internal_roll(roll, cursor)?;
            cursor = remaining_capacity;
            roll_ptrs.push(ptr);
        }
        Ok(roll_ptrs)
    }
}
