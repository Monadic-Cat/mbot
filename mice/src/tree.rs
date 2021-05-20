//! Tree traversal utilities.
use crate::parse::new::{Program, Term};
use ::id_arena::{Arena, Id};

#[derive(Copy, Clone)]
struct TreeIndex(u8);

#[derive(Clone)]
struct StackFrame {
    cursor: TreeIndex,
    id: Id<Term>,
}
impl StackFrame {
    fn new(cursor: TreeIndex, id: Id<Term>) -> Self {
        Self { cursor, id }
    }
}

/// Program tree walker.
pub struct PTreeWalker<'a> {
    terms: &'a Arena<Term>,
    stack: Vec<StackFrame>,
    cursor: TreeIndex,
}

impl<'a> PTreeWalker<'a> {
    pub fn new(program: &Program) -> PTreeWalker<'_> {
        PTreeWalker {
            terms: &program.terms,
            stack: vec![StackFrame::new(TreeIndex(0), program.top)],
            cursor: TreeIndex(0),
        }
    }
    pub fn stack_top(&self) -> Option<&'a Term> {
        self.stack.last().map(|StackFrame { id, .. }| &self.terms[*id])
    }

    fn index_term(&self, term: &Term, index: TreeIndex) -> Option<Id<Term>> {
        let TreeIndex(index) = index;
        match (index, term) {
            (_, Term::Constant(_)) | (_, Term::DiceRoll(_, _)) => None,
            (0, Term::KeepHigh(a, _)) => Some(*a),
            (0, Term::Add(a, _)) | (0, Term::Subtract(a, _)) => Some(*a),
            (0, Term::UnaryAdd(a)) | (0, Term::UnarySubtract(a)) => Some(*a),
            (1, Term::KeepHigh(_, _)) => None,
            (1, Term::Add(_, b)) | (1, Term::Subtract(_, b)) => Some(*b),
            (1, Term::UnaryAdd(_)) | (1, Term::UnarySubtract(_)) => None,
            (2..=255, _) => None,
        }
    }

    pub fn current(&self) -> Option<&'a Term> {
        self.stack_top().map(|top| self.index_term(top, self.cursor).map(|id| &self.terms[id])).flatten()
    }

    pub fn descend(&mut self) -> Result<(), ()> {
        let stack_top = match self.stack_top() {
            Some(x) => x,
            None => return Err(()),
        };
        let child = match self.index_term(stack_top, self.cursor) {
            Some(x) => x,
            None => return Err(()),
        };
        self.stack.push(StackFrame::new(self.cursor, child));
        self.cursor = TreeIndex(0);
        Ok(())
    }
    pub fn ascend(&mut self) -> Result<(), ()> {
        // No ascending past the top of the tree.
        if self.stack.len() > 1 {
            let StackFrame { cursor, id: _ } = match self.stack.pop() {
                Some(x) => x,
                None => unreachable!("we just checked the stack's length"),
            };
            self.cursor = cursor;
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn move_left(&mut self) {
        let TreeIndex(cursor) = self.cursor;
        self.cursor = TreeIndex(cursor.saturating_sub(1));
    }
    pub fn move_right(&mut self) {
        let TreeIndex(cursor) = self.cursor;
        self.cursor = TreeIndex(cursor.saturating_add(1));
    }

    pub fn has_children(&self) -> bool {
        {
            // If these variants start having children, this will fail to compile,
            // which will be a reminder to adjust this function.
            const _: Term = Term::Constant(0);
            const _: Term = Term::DiceRoll(0, 0);
        }
        let current = match self.current() {
            Some(x) => x,
            None => return false,
        };
        match current {
            Term::Constant(_) | Term::DiceRoll(_, _) => false,
            Term::KeepHigh(_, _)
            | Term::Add(_, _)
            | Term::Subtract(_, _)
            | Term::UnaryAdd(_)
            | Term::UnarySubtract(_) => true,
        }
    }

    pub fn ancestors(&self) -> AncestorsIter<'_> {
        AncestorsIter {
            terms: self.terms,
            stack: &*self.stack,
            idx: Some(self.stack.len() - 1),
        }
    }
}

/// Using the principle of explosion, we can synthesize any type.
fn explosion<T>() -> T {
    panic!("Boom!")
}

pub struct AncestorsIter<'a> {
    terms: &'a Arena<Term>,
    stack: &'a [StackFrame],
    idx: Option<usize>,
}
impl<'a> AncestorsIter<'a> {
    // For an empty iterator, we don't actually need
    // to hold a reference to a terms arena,
    // but I didn't want to bother unwrapping an Option for it.
    fn empty(terms: &'a Arena<Term>) -> AncestorsIter<'a> {
        AncestorsIter {
            stack: &[],
            idx: None,
            terms,
        }
    }
}
impl<'a> Iterator for AncestorsIter<'a> {
    type Item = &'a Term;
    fn next(&mut self) -> Option<Self::Item> {
        match self.idx {
            Some(idx) => {
                let item = &self.terms[self.stack[idx].id];
                self.idx = idx.checked_sub(1);
                Some(item)
            },
            None => None,
        }
    }
}

/// Currently has substantially more iteration overhead than [`postorder`](crate::stack::postorder),
/// but should running time appears to stay inside an order of magnitude on large trees.
pub fn postorder(program: &Program) -> PostorderIter<'_> {
    let mut walker = PTreeWalker::new(program);
    while let Ok(()) = walker.descend() {}
    PostorderIter { walker: Some(walker) }
}

pub struct PostorderIter<'a> {
    walker: Option<PTreeWalker<'a>>,
}

// An iterator trait that supports borrowing from the iterator,
// by allowing the argument lifetime to be named inside the associated type.
pub trait StreamingIterator<'a> {
    type Item;
    fn next(&'a mut self) -> Option<Self::Item>;
}

impl<'arena, 'iter> StreamingIterator<'iter> for PostorderIter<'arena> where 'arena: 'iter {
    type Item = (&'arena Term, AncestorsIter<'iter>);
    fn next(&'iter mut self) -> Option<Self::Item> {
        match self.walker.as_mut() {
            Some(walker) => {
                let current = walker.current();
                match current {
                    Some(current) => {
                        walker.move_right();
                        Some((current, walker.ancestors()))
                    },
                    None => {
                        match walker.ascend() {
                            // We had a parent. Use it.
                            Ok(()) => {
                                let current = walker.current().unwrap();
                                walker.move_right();
                                Some((current, walker.ancestors()))
                            },
                            // We tried to ascend past the root, which is our current parent.
                            // Use the root and destroy the walker.
                            Err(()) => {
                                let current = walker.stack_top();
                                let terms = walker.terms;
                                // let's, uh, just ignore borrowck for a moment
                                // (this is what happens when I don't forbid unsafe code)
                                // Safety: `walker` doesn't live to this point.
                                // Borrowck just isn't smart enough to see that the borrow
                                // dies sooner in this branch.
                                // Additionally, Option<PTreeWalker> is guaranteed to have
                                // the same layout as PTreeWalker, and so the same starting
                                // address, and so writing None through this pointer cast
                                // will be valid.
                                // For extra validation, but not necessarily guarantee,
                                // Miri doesn't complain about this on
                                // `miri 0.1.0 (bcae331 2021-05-12)`.
                                let ptr = walker as *mut PTreeWalker as *mut Option<PTreeWalker>;
                                unsafe {
                                    *ptr = None;
                                }
                                current.map(|current| (current, AncestorsIter::empty(terms)))
                            },
                        }
                    }
                }
            },
            None => None,
        }
    }
}

impl<'arena> PostorderIter<'arena> {
    pub fn next<'iter>(&'iter mut self) -> Option<(&'arena Term, AncestorsIter<'iter>)>
    where PostorderIter<'arena>: StreamingIterator<'iter, Item = (&'arena Term, AncestorsIter<'iter>)> {
        <Self as StreamingIterator<'iter>>::next(self)
    }
}

/// This is a module whose contents I will not at all try to keep stable,
/// consisting entirely of things that are only `pub` so that I can use
/// them from inside the outputs of macros that I *do* want public.
#[doc(hidden)]
pub mod private_for_inside_macro_outputs {
    pub use ::core::option::Option::Some;
}
/// A `for` loop-alike for [`StreamingIterator`].
/// ```
/// # use ::mice::tree::for_;
/// use ::mice::parse::new::parse_expression;
/// let program = parse_expression("4d6k3 + 2".as_bytes()).unwrap().1;
/// for_! { (term, _ancestors) in ::mice::tree::postorder(&program) => {
///     // do stuff with `term` and maybe `_ancestors`
/// }}
/// ```
#[macro_export]
macro_rules! for_ {
    ($elem:pat in $iter:expr => $blk:block) => {
        match $iter {
            mut iter =>
                while let $crate::tree::private_for_inside_macro_outputs::Some($elem)
                = $crate::tree::StreamingIterator::next(&mut iter) $blk
        }
    }
}
#[doc(inline)]
pub use for_;

// Convenience impl so we can use normal iterators with our fake `for` loop, too.
// Also, demonstrates that the only difference between Iterator and our
// StreamingIterator is that we can name the lifetime of the borrow at `.next()`
// inside the associated type. In theory, you could implement StreamingIterator
// with only a few specific lifetimes, but the only lifetime you can name outside
// of a universal quantifier is the `'static` lifetime.
impl<'a, Iter, T> StreamingIterator<'a> for Iter
where Iter: Iterator<Item = T> {
    type Item = T;
    fn next(&'a mut self) -> Option<Self::Item> {
        <Self as Iterator>::next(self)
    }
}

#[cfg(test)]
#[test]
fn it_works() {
    let program = crate::parse::new::parse_expression("4d6k3 + 2".as_bytes()).unwrap().1;
    let mut iter = postorder(&program);
    while let Some((term, ancestors)) = iter.next() {
        dbg!(term);
        for term in ancestors {
            println!("Ancestor: {:?}", term);
        }
    }
}
