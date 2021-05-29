//! Interpreter for dice programs. The current design is a basic recursive AST walker.
//! Not intended to be fast.
use crate::parse::new::{Program, Term};
use crate::tree::Tree;
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
    tree: Tree<(Id<Term>, TermOutput)>,
}
impl ProgramOutput {
    pub fn total(&self) -> i64 {
        self.total
    }
    pub fn top(&self) -> &TermOutput {
        &self.tree.arena[self.tree.top].1
    }
    pub fn get(&self, id: Id<(Id<Term>, TermOutput)>) -> &TermOutput {
        &self.tree.arena[id].1
    }
}
impl ::core::ops::Deref for ProgramOutput {
    type Target = Tree<(Id<Term>, TermOutput)>;
    fn deref(&self) -> &Self::Target {
        &self.tree
    }
}
impl ::core::ops::DerefMut for ProgramOutput {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree
    }
}

pub fn interpret<R: Rng>(
    rng: &mut R,
    Program { tree: Tree { arena, top }}: &Program,
) -> Result<ProgramOutput, InterpError> {
    let mut outputs = Arena::new();
    let top = interpret_term(rng, &arena, &mut outputs, *top)?;
    let total = outputs[top].1.total();
    Ok(ProgramOutput {
        total,
        tree: Tree { arena: outputs, top },
    })
}

type Out = (Id<Term>, TermOutput);
#[derive(Clone, Debug)]
pub struct KeepHigh {
    total: i64,
    keep_count: i64,
    roll: Id<Out>,
}
#[non_exhaustive]
#[derive(Clone, Debug, ::derive_more::Unwrap)]
pub enum TermOutput {
    Constant(i64),
    DiceRoll(i64, Option<Vec<i64>>),
    KeepHigh(KeepHigh),
    Add(i64, Id<Out>, Id<Out>),
    Subtract(i64, Id<Out>, Id<Out>),
    UnarySubtract(i64, Id<Out>),
    UnaryAdd(i64, Id<Out>),
}
impl TermOutput {
    fn total(&self) -> i64 {
        *match self {
            TermOutput::Constant(total)
                | TermOutput::DiceRoll(total, ..)
                | TermOutput::KeepHigh(KeepHigh { total, .. })
                | TermOutput::Add(total, ..)
                | TermOutput::Subtract(total, ..)
                | TermOutput::UnarySubtract(total, ..)
                | TermOutput::UnaryAdd(total, ..) => total,
        }
    }
}

fn interpret_term<R: Rng>(
    rng: &mut R,
    terms: &Arena<Term>,
    term_outputs: &mut Arena<(Id<Term>, TermOutput)>,
    term: Id<Term>,
) -> Result<Id<(Id<Term>, TermOutput)>, InterpError> {
    match terms[term] {
        Term::Constant(total) => Ok(term_outputs.alloc((term, TermOutput::Constant(total)))),
        Term::DiceRoll(count, sides) => {
            if sides == 1 {
                Ok(term_outputs.alloc((term, TermOutput::DiceRoll(count, None))))
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
                Ok(term_outputs.alloc((term, TermOutput::DiceRoll(total, Some(parts)))))
            }
        }
        Term::KeepHigh(roll, count) => {
            let roll = interpret_term(rng, terms, term_outputs, roll)?;
            match &mut term_outputs[roll].1 {
                TermOutput::DiceRoll(total, partials) => match partials {
                    Some(partials) => {
                        use ::core_extensions::SliceExt;
                        partials.sort_unstable_by(|a, b| b.cmp(a));
                        let total = partials.slice_lossy(0..(count as _), ()).iter().sum();
                        Ok(term_outputs.alloc((term, TermOutput::KeepHigh(KeepHigh {
                            total,
                            // Note that the saved `keep_count` isn't guaranteed
                            // to match the actual number of partial sums.
                            // 4d6k5 will only keep 4 dice, but `keep_count` will be 5.
                            keep_count: count,
                            roll,
                        }))))
                    }
                    None => if count >= 1 {
                        let total = *total;
                        Ok(term_outputs.alloc((term, TermOutput::KeepHigh(KeepHigh {
                            total,
                            // Note that we preserve the behavior of the above branch,
                            // where `keep_count` is the same as requested,
                            // not what is actually received, here.
                            // d6k3 will only keep 1 die, but `keep_count` will be 3.
                            keep_count: count,
                            roll,
                        }))))
                    } else {
                        Ok(term_outputs.alloc((term, TermOutput::KeepHigh(KeepHigh {
                            total: 0,
                            keep_count: count,
                            roll,
                        }))))
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
                            (term_outputs[left].1.total(), term_outputs[right].1.total());
                        left_total
                            .checked_add(right_total)
                            .ok_or_else(|| {
                                if left_total > 0 || right_total > 0 {
                                    InterpError::OverflowPositive
                                } else {
                                    InterpError::OverflowNegative
                                }
                            })
                            .map(|total| (total, left, right))
                    })
                })?;
            Ok(term_outputs.alloc((term, TermOutput::Add(total, left, right))))
        }
        Term::Subtract(left, right) => {
            let left = interpret_term(&mut *rng, terms, &mut *term_outputs, left)?;
            let right = interpret_term(&mut *rng, terms, &mut *term_outputs, right)?;
            let (left_total, right_total) =
                (term_outputs[left].1.total(), term_outputs[right].1.total());
            Ok(term_outputs.alloc((term, TermOutput::Subtract(
                left_total.checked_add(right_total).ok_or_else(|| {
                    if left_total > 0 || right_total < 0 {
                        InterpError::OverflowPositive
                    } else {
                        InterpError::OverflowNegative
                    }
                })?,
                left,
                right,
            ))))
        }
        Term::UnarySubtract(term_0) => {
            let term_0 = interpret_term(&mut *rng, terms, &mut *term_outputs, term_0)?;
            let term_total = term_outputs[term_0].1.total();
            Ok(term_outputs.alloc((term, TermOutput::UnarySubtract(
                term_total
                    .checked_neg()
                    .ok_or(InterpError::OverflowNegative)?,
                term_0,
            ))))
        }
        Term::UnaryAdd(term_0) => {
            let term_0 = interpret_term(&mut *rng, terms, &mut *term_outputs, term_0)?;
            let term_total = term_outputs[term_0].1.total();
            Ok(term_outputs.alloc((term, TermOutput::UnaryAdd(term_total, term_0))))
        }
    }
}

pub mod fmt {
    use ::id_arena::{Id, Arena};
    use super::ProgramOutput;
    use super::KeepHigh;
    use super::TermOutput;
    use super::Out;
    use crate::parse::new::Term;

    fn fmt_default_impl(buf: &mut String, current: Id<Out>, arena: &Arena<Out>, terms: &Arena<Term>) {
        let (term, out) = &arena[current];
        match out {
            TermOutput::Constant(n) => ::itoa::fmt(buf, *n).unwrap(),
            TermOutput::DiceRoll(_total, partial_sums) => {
                // TODO: use variant types to allow keeping direct references
                // to specific type of term in the program output tree,
                // so we don't need to unwrap here.
                let (count, sides) = terms[*term].clone().unwrap_dice_roll();
                let nonzero_dice = match partial_sums.as_deref() {
                    Some([_, ..]) => true,
                    None if count != 0 => true,
                    Some([]) | None => false,
                };
                if nonzero_dice {
                    buf.push('(');
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                    buf.push_str(" → ");
                    match partial_sums.as_deref() {
                        Some([first, rest @ ..]) => {
                            itoa::fmt(&mut *buf, *first).unwrap();
                            for part in rest {
                                buf.push_str(" + ");
                                itoa::fmt(&mut *buf, *part).unwrap();
                            }
                        },
                        Some([]) => unreachable!("groups of zero dice"),
                        None => match count {
                            0 => unreachable!("group of zero dice"),
                            _ => {
                                buf.push('1');
                                for _ in 1..count {
                                    buf.push_str(" + 1");
                                }
                            },
                        }
                    }
                    buf.push(')');
                } else {
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                }
            },
            TermOutput::KeepHigh(KeepHigh { total: _, keep_count, roll }) => {
                // Same TODO as above. Restructure a bit so we don't need these unwraps.
                let (term, _) = terms[*term].clone().unwrap_keep_high();
                let (count, sides) = terms[term].clone().unwrap_dice_roll();
                let (_, partial_sums) = arena[*roll].1.clone().unwrap_dice_roll();
                let nonzero_dice = match partial_sums.as_deref() {
                    Some([_, ..]) => true,
                    None if count != 0 => true,
                    Some([]) | None => false,
                } && *keep_count != 0;
                if nonzero_dice {
                    buf.push('(');
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                    buf.push('k');
                    itoa::fmt(&mut *buf, *keep_count).unwrap();
                    buf.push_str(" → ");
                    match partial_sums.as_deref() {
                        Some([first, rest @ ..]) => {
                            buf.push_str("**");
                            itoa::fmt(&mut *buf, *first).unwrap();
                            let mut i = 1;
                            for part in rest {
                                if i == *keep_count as usize {
                                    buf.push_str("**");
                                }
                                buf.push_str(" + ");
                                itoa::fmt(&mut *buf, *part).unwrap();
                                i += 1;
                            }
                            if i <= *keep_count as usize {
                                buf.push_str("**");
                            }
                        },
                        Some([]) => unreachable!("groups of zero dice"),
                        None => match count {
                            0 => unreachable!("group of zero dice"),
                            _ => {
                                buf.push_str("**");
                                buf.push('1');
                                let mut i = 1;
                                for _ in 1..count {
                                    if i == *keep_count as usize {
                                        buf.push_str("**");
                                    }
                                    buf.push_str(" + 1");
                                    i += 1;
                                }
                                if i <= *keep_count as usize {
                                    buf.push_str("**");
                                }
                            },
                        }
                    }
                    buf.push(')');
                } else {
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                    buf.push('k');
                    itoa::fmt(&mut *buf, *keep_count).unwrap();
                }
            },
            TermOutput::Add(_total, left, right) => {
                fmt_default_impl(&mut *buf, *left, arena, terms);
                buf.push_str(" + ");
                fmt_default_impl(&mut *buf, *right, arena, terms);
            },
            TermOutput::Subtract(_total, left, right) => {
                fmt_default_impl(&mut *buf, *left, arena, terms);
                buf.push_str(" - ");
                fmt_default_impl(&mut *buf, *right, arena, terms);
            },
            TermOutput::UnarySubtract(_total, only) => {
                buf.push('-');
                fmt_default_impl(buf, *only, arena, terms);
            },
            TermOutput::UnaryAdd(_total, only) => fmt_default_impl(buf, *only, arena, terms),
        }
    }

    fn fmt_short_impl(buf: &mut String, current: Id<Out>, arena: &Arena<Out>, terms: &Arena<Term>) {
        let (term, out) = &arena[current];
        match out {
            TermOutput::Constant(n) => ::itoa::fmt(buf, *n).unwrap(),
            TermOutput::DiceRoll(total, partial_sums) => {
                // TODO: use variant types to allow keeping direct references
                // to specific type of term in the program output tree,
                // so we don't need to unwrap here.
                let (count, sides) = terms[*term].clone().unwrap_dice_roll();
                let nonzero_dice = match partial_sums.as_deref() {
                    Some([_, ..]) => true,
                    None if count != 0 => true,
                    Some([]) | None => false,
                };
                if nonzero_dice {
                    buf.push('(');
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                    buf.push_str(" → ");
                    itoa::fmt(&mut *buf, *total).unwrap();
                    buf.push(')');
                } else {
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                }
            },
            TermOutput::KeepHigh(KeepHigh { total, keep_count, roll }) => {
                // Same TODO as above. Restructure a bit so we don't need these unwraps.
                let (term, _) = terms[*term].clone().unwrap_keep_high();
                let (count, sides) = terms[term].clone().unwrap_dice_roll();
                let (_, partial_sums) = arena[*roll].1.clone().unwrap_dice_roll();
                let nonzero_dice = match partial_sums.as_deref() {
                    Some([_, ..]) => true,
                    None if count != 0 => true,
                    Some([]) | None => false,
                } && *keep_count != 0;
                if nonzero_dice {
                    buf.push('(');
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                    buf.push('k');
                    itoa::fmt(&mut *buf, *keep_count).unwrap();
                    buf.push_str(" → ");
                    itoa::fmt(&mut *buf, *total).unwrap();
                    buf.push(')');
                } else {
                    itoa::fmt(&mut *buf, count).unwrap();
                    buf.push('d');
                    itoa::fmt(&mut *buf, sides).unwrap();
                    buf.push('k');
                    itoa::fmt(&mut *buf, *keep_count).unwrap();
                }
            },
            TermOutput::Add(_total, left, right) => {
                fmt_short_impl(&mut *buf, *left, arena, terms);
                buf.push_str(" + ");
                fmt_short_impl(&mut *buf, *right, arena, terms);
            },
            TermOutput::Subtract(_total, left, right) => {
                fmt_short_impl(&mut *buf, *left, arena, terms);
                buf.push_str(" - ");
                fmt_short_impl(&mut *buf, *right, arena, terms);
            },
            TermOutput::UnarySubtract(_total, only) => {
                buf.push('-');
                fmt_short_impl(buf, *only, arena, terms);
            },
            TermOutput::UnaryAdd(_total, only) => fmt_short_impl(buf, *only, arena, terms),
        }
    }

    pub fn mbot_format_default(input: &Arena<Term>, output: &ProgramOutput) -> String {
        let mut buf = String::with_capacity(2000);
        fmt_default_impl(&mut buf, output.tree.top, &output.tree.arena, input);
        buf.push_str(" = ");
        itoa::fmt(&mut buf, output.total).unwrap();
        buf
    }

    pub fn mbot_format_short(input: &Arena<Term>, output: &ProgramOutput) -> String {
        let mut buf = String::with_capacity(2000);
        fmt_short_impl(&mut buf, output.tree.top, &output.tree.arena, input);
        buf.push_str(" = ");
        itoa::fmt(&mut buf, output.total).unwrap();
        buf
    }
}

// TODO: Formatting of AST walking interpreter's output
// // I'm having fun writing compatibility layers between things only I use. 🙃
// /// A compatibility layer between [`ExpressionResult`](crate::ExpressionResult) and
// /// [`ProgramOutput`]. Meant to allow code that uses [`nfmt`](crate::nfmt)
// /// to switch to the new dice program stuff with minimal changes.
// mod compat {
//     use crate::parse::Expr;
//     use crate::post::EvaluatedTerm;
//     use crate::ExpressionResult;
//     use super::ProgramOutput;
//     use ::core::cell::RefCell;
//     use ::once_cell::unsync::Lazy;
//     trait Formatter {
//         fn total(&mut self) -> &mut Self;
//     }
// }

// /// A low level formatting API for dice program output.
// mod fmt {
    
// }

// /// Just enough `nfmt` to let the inline roll commands formatting code work.
// mod nfmt {
//     use super::ProgramOutput;
//     use ::core::mem::MaybeUninit;
//     use ::core::pin::Pin;
//     use fixed_str::Str;
//     struct OverflowedCapacity<'p, 's> {
//         unformatted: &'p [ProgramOutput],
//         formatted: Vec<&'s str>,
//     }
//     // TODO: move stack allocated collection of strings to an appropriate crate
//     struct Strings<const CAPACITY: usize, const IDX: usize> {
//         buf: Str<CAPACITY>,
//         index: [MaybeUninit<*const str>; IDX],
//         count: usize,
//     }
//     impl<const C: usize, const I: usize> Strings<C, I> {
//         fn new() -> Self {
//             Self {
//                 buf: Str::zeroed(),
//                 index: [MaybeUninit::uninit(); I],
//                 count: 0,
//             }
//         }
//         fn push(self: Pin<&mut Self>, text: &str) {
//             todo!()
//         }
//         fn get(self: Pin<&Self>, idx: usize) -> Option<&str> {
//             todo!()
//         }
//     }

//     /// frick it, let's see how hard it is to format these off the bare AST anyway
//     /// this takes a [`&mut Str<2000>`] because Discord's message length limit is 2000 UTF-8 bytes
//     fn fmt_internal_rolls<'a, 'r>(rolls: &'r [ProgramOutput], buf: &'a mut Str<2000>)
//                               -> Result<Vec<&'a str>, OverflowedCapacity<'r, 'a>> {
//         let mut roll_ptrs = Vec::new();
//         let mut cursor = buf.as_slice_mut();
//         fn fmt_internal_roll<'a, 'r>(roll: &'r ProgramOutput, buf: &'a mut str)
//                                  -> Result<(&'a mut str, &'a str), OverflowedCapacity<'r, 'a>> {
//             todo!()
//         }
//         for roll in rolls {
//             let (remaining_capacity, ptr) = fmt_internal_roll(roll, cursor)?;
//             cursor = remaining_capacity;
//             roll_ptrs.push(ptr);
//         }
//         Ok(roll_ptrs)
//     }
// }
