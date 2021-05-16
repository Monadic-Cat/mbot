//! A stack machine for dice programs. Currently just interprets the stack machine bytecode,
//! but is already faster than the other two interpreters in all benchmarks.
use crate::parse::new::{Program, Term};
use ::id_arena::Arena;
use ::rand::Rng;


/// Require dice roll terms are terminal to compile.
macro_rules! assert_dice_roll_terminal {
    () => {{
        // For this to be a correct postorder traversal,
        // DiceRoll must have no child nodes.
        // Since I'm considering making dice rolling a true
        // operator in the future, we ensure there will be a type
        // error here as a reminder to fix up the tree traveral
        // code when the time comes.
        const _: $crate::parse::new::Term = $crate::parse::new::Term::DiceRoll(0i64, 0i64);
    }};
}

/// Perform a postorder traveral of a program's AST.
pub fn postorder<F>(program: &Program, mut visit: F)
where
    F: FnMut(&Term, Option<&Term>),
{
    let Program { top, terms } = program;
    fn postorder_term<F>(term: &Term, parent: Option<&Term>, arena: &Arena<Term>, visit: &mut F)
    where
        F: FnMut(&Term, Option<&Term>),
    {
        use Term::*;
        assert_dice_roll_terminal!();
        match term {
            node @ Constant(_) => visit(node, parent),
            node @ DiceRoll(_, _) => visit(node, parent),
            KeepHigh(roll, _) => {
                visit(&arena[*roll], Some(term));
                visit(term, parent);
            },
            Add(left, right) | Subtract(left, right) => {
                postorder_term(&arena[*left], Some(term), arena, &mut *visit);
                postorder_term(&arena[*right], Some(term), arena, &mut *visit);
                visit(term, parent);
            },
            UnaryAdd(only) | UnarySubtract(only) => {
                postorder_term(&arena[*only], Some(term), arena, &mut *visit);
                visit(term, parent);
            },
        }
    }
    postorder_term(&terms[*top], None, terms, &mut visit);
}

pub struct StackProgram(Vec<Instruction>);
#[derive(Copy, Clone)]
enum Instruction {
    Value(i64),
    DiceRoll(i64, i64),
    // This currently gets a special instruction so we can maintain performance.
    DiceRollKeepHigh {
        count: i64,
        sides: i64,
        keep_count: i64,
    },
    Add,
    Subtract,
    UnarySubtract,
    NoOp,
}

/// Compile a dice program to run on the stack machine.
pub fn compile(program: &Program) -> StackProgram {
    let mut instructions = Vec::with_capacity(program.terms.len());
    postorder(program, |term, parent| {
        use Term::*;
        let next = match term {
            Constant(value) => Instruction::Value(*value),
            DiceRoll(count, sides) => match parent {
                Some(KeepHigh(_, keep_count)) => Instruction::DiceRollKeepHigh {
                    count: *count,
                    sides: *sides,
                    keep_count: *keep_count,
                },
                _ => Instruction::DiceRoll(*count, *sides),
            },
            KeepHigh(_, _) => Instruction::NoOp,
            Add(_, _) => Instruction::Add,
            Subtract(_, _) => Instruction::Subtract,
            // Unary addition is a no-op, so we compile it to one.
            // If I were slightly less lazy, these would just not be pushed
            // into the instruction listing.
            UnaryAdd(_) => Instruction::NoOp,
            UnarySubtract(_) => Instruction::UnarySubtract,
        };
        instructions.push(next);
    });
    StackProgram(instructions)
}

pub struct Machine {
    stack: Vec<i64>,
}

pub enum Overflow {
    Positive,
    Negative,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
        }
    }
    fn exec_with<R: Rng>(&mut self, rng: &mut R, instruction: Instruction) -> Result<(), Overflow> {
        use Instruction::*;
        match instruction {
            Value(value) => self.stack.push(value),
            DiceRoll(count, sides) => {
                if sides == 1 {
                    self.stack.push(count);
                } else {
                    let mut total: i64 = 0;
                    for _ in 0..count {
                        // TODO: have way to save partial sums
                        let random = rng.gen_range(0, sides) + 1;
                        total = total.checked_add(random)
                            .ok_or(Overflow::Positive)?;
                    }
                    self.stack.push(total);
                }
            },
            DiceRollKeepHigh { count, sides, keep_count } => {
                if keep_count == 0 {
                    self.stack.push(0);
                } else if sides == 1 {
                    self.stack.push(::core::cmp::min(count, keep_count));
                } else {
                    let mut partials = Vec::<i64>::with_capacity(keep_count as _);
                    for _ in 0..count {
                        // TODO: have way to save partial sums
                        let random = rng.gen_range(0, sides) + 1;
                        partials.push(random);
                        partials.sort_unstable_by(|a, b| b.cmp(a));
                        partials.truncate(keep_count as _);
                    }
                    let total: i64 = partials.iter().sum();
                    self.stack.push(total);
                }
            },
            Add => {
                // Note: These are guaranteed to exist due to how the tree is constructed.
                let (left, right) = (self.stack.pop().unwrap(), self.stack.pop().unwrap());
                self.stack.push(match left.checked_add(right) {
                    Some(x) => x,
                    None => {
                        if left > 0 || right > 0 {
                            return Err(Overflow::Positive)
                        } else {
                            return Err(Overflow::Negative)
                        }
                    }
                });
            }
            Subtract => {
                // Note: These are guaranteed to exist due to how the tree is constructed.
                let (left, right) = (self.stack.pop().unwrap(), self.stack.pop().unwrap());
                self.stack.push(match left.checked_sub(right) {
                    Some(x) => x,
                    None => {
                        if left > 0 || right < 0 {
                            return Err(Overflow::Positive)
                        } else {
                            return Err(Overflow::Negative)
                        }
                    }
                });
            }
            UnarySubtract => {
                // Note: This is guaranteed to exist due to how the tree is constructed.
                let only = self.stack.pop().unwrap();
                self.stack.push(only.checked_neg().ok_or(Overflow::Positive)?);
            }
            NoOp => (),
        }
        Ok(())
    }
    fn run_with<R: Rng>(&mut self, rng: &mut R, program: &StackProgram) -> Result<(), Overflow>{
        for instruction in program.0.iter() {
            self.exec_with(&mut *rng, *instruction)?;
        }
        Ok(())
    }
    pub fn eval_with<R: Rng>(&mut self, rng: &mut R, program: &StackProgram) -> Result<i64, Overflow> {
        match self.run_with(rng, program) {
            // Note: This is guaranteed to exist due to how the tree is constructed,
            // and thereby how successful execution must have proceeded.
            // Further note that this will be the last value left on the stack by the
            // just now evaluated program. So, no clearing is necessary.
            Ok(()) => Ok(self.stack.pop().unwrap()),
            Err(overflow) => {
                // TODO: consider not clearing the stack, for debugging purposes
                // In such a case, we might instead consider exposing a method
                // to clear the stack directly.
                self.stack.clear();
                Err(overflow)
            }
        }
    }
}
