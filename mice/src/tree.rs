//! Tree traversal utilities.
use crate::parse::new::Term;
use ::id_arena::{Arena, Id};

/// A generic tree structure, with nodes of type `T`.
#[derive(Debug)]
pub struct Tree<T> {
    pub(crate) arena: Arena<T>,
    pub(crate) top: Id<T>,
}
impl<T> Tree<T> {
    /// Create a tree walker to arbitrarily traverse the tree.
    pub fn walk(&self) -> TreeWalker<'_, T> {
        TreeWalker {
            arena: &self.arena,
            stack: vec![StackFrame::new(TreeIndex(0), self.top)],
            cursor: TreeIndex(0),
        }
    }

    /// Perform a postorder depth first traversal of the tree.
    /// Currently has substantially more iteration overhead than [`postorder`](crate::stack::postorder),
    /// but running time appears to stay inside an order of magnitude on large trees.
    pub fn postorder(&self) -> PostorderIter<'_, T>
        where T: IndexNode,
    {
        let mut walker = self.walk();
        while let Ok(()) = walker.descend() {}
        PostorderIter { walker: Some(walker) }
    }
    // TODO: add recursive implementation of the postorder walk
}

// TODO: possibly rename this something more broad, to more appropriately have
// `has_children` and `iter` methods.
pub trait IndexNode: Sized {
    fn index(&self, index: TreeIndex) -> Option<Id<Self>>;
    fn has_children(&self) -> bool {
        self.index(TreeIndex(0)).is_some()
    }
    // TODO: add an `iter` method for shallow iteration of a node's children,
    // with a default implementation in terms of `index`.
}
impl IndexNode for Term {
    fn index(&self, index: TreeIndex) -> Option<Id<Self>> {
        {
            // If these variants start having children, this will fail to compile,
            // which will be a reminder to adjust this function.
            const _: Term = Term::Constant(0);
            const _: Term = Term::DiceRoll(0, 0);
        }
        let TreeIndex(index) = index;
        match (index, self) {
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
}

struct StackFrame<T> {
    cursor: TreeIndex,
    id: Id<T>,
}
impl<T> StackFrame<T> {
    fn new(cursor: TreeIndex, id: Id<T>) -> Self {
        Self { cursor, id }
    }
}

pub struct TreeWalker<'a, T> {
    arena: &'a Arena<T>,
    stack: Vec<StackFrame<T>>,
    cursor: TreeIndex,
}

impl<'a, T> TreeWalker<'a, T> {
    pub fn stack_top(&self) -> Option<&'a T> {
        self.stack.last().map(|StackFrame { id, .. }| &self.arena[*id])
    }
    pub fn current(&self) -> Option<&'a T>
        where T: IndexNode,
    {
        self.stack_top().map(|top| top.index(self.cursor).map(|id| &self.arena[id])).flatten()
    }

    pub fn descend(&mut self) -> Result<(), ()>
        where T: IndexNode,
    {
        let stack_top = match self.stack_top() {
            Some(x) => x,
            None => return Err(()),
        };
        let child = match stack_top.index(self.cursor) {
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

    pub fn has_children(&self) -> bool
        where T: IndexNode,
    {
        match self.current() {
            Some(x) => x.has_children(),
            None => false,
        }
    }

    pub fn ancestors(&self) -> AncestorsIter<'_, T> {
        AncestorsIter {
            arena: self.arena,
            stack: &*self.stack,
            idx: Some(self.stack.len() - 1)
        }
    }
}

pub struct AncestorsIter<'a, T> {
    arena: &'a Arena<T>,
    stack: &'a [StackFrame<T>],
    idx: Option<usize>,
}
impl<'a, T> AncestorsIter<'a, T> {
    // For an empty iterator, we don't actually need
    // to hold a reference to a terms arena,
    // but I didn't want to bother unwrapping an Option for it.
    fn empty(arena: &'a Arena<T>) -> AncestorsIter<'a, T> {
        AncestorsIter {
            stack: &[],
            idx: None,
            arena,
        }
    }
}
impl<'a, T> Iterator for AncestorsIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.idx {
            Some(idx) => {
                let item = &self.arena[self.stack[idx].id];
                self.idx = idx.checked_sub(1);
                Some(item)
            },
            None => None,
        }
    }
}

pub struct PostorderIter<'a, T> {
    walker: Option<TreeWalker<'a, T>>,
}
impl<'arena, 'iter, T> StreamingIterator<'iter> for PostorderIter<'arena, T>
where 'arena: 'iter,
      T: IndexNode,
{
    type Item = (&'arena T, AncestorsIter<'iter, T>);
    fn next(&'iter mut self) -> Option<Self::Item> {
        match self.walker.as_mut() {
            Some(walker) => {
                let current = walker.current();
                match current {
                    Some(current) => {
                        walker.move_right();
                        // Attempt to move downward when moving right.
                        while let Ok(()) = walker.descend() {}
                        Some((current, walker.ancestors()))
                    },
                    None => {
                        match walker.ascend() {
                            // We had a parent. Use it.
                            Ok(()) => {
                                let current = walker.current().unwrap();
                                walker.move_right();
                                // Attempt to move downward when moving right.
                                while let Ok(()) = walker.descend() {}
                                Some((current, walker.ancestors()))
                            },
                            // We tried to ascend past the root, which is our current parent.
                            // Use the root and destroy the walker.
                            Err(()) => {
                                let current = walker.stack_top();
                                let terms = walker.arena;
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
                                let ptr = walker as *mut TreeWalker<T> as *mut Option<TreeWalker<T>>;
                                unsafe {
                                    *ptr = None;
                                }
                                // This passes with -Zpolonius:
                                // self.walker = None;
                                // So, presumably, we'll be able to remove this hack once full NLL
                                // makes it into rustc. Which, of course, means that
                                // it's almost definitely safe to do this thing we're doing here.
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

// TODO: make TreeIndex generic over node type,
// such that the index width is decided by the maximum number
// of children a tree node may have
#[derive(Copy, Clone)]
pub struct TreeIndex(u8);

/// Using the principle of explosion, we can synthesize any type.
fn explosion<T>() -> T {
    panic!("Boom!")
}

/// An iterator trait that supports borrowing from the iterator,
/// by allowing the argument lifetime to be named inside the associated type.
pub trait StreamingIterator<'a> {
    type Item;
    fn next(&'a mut self) -> Option<Self::Item>;
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
/// let (_tokens, program) = parse_expression("4d6k3 + 2".as_bytes()).unwrap().1;
/// for_! { (term, _ancestors) in program.postorder() => {
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
    let (_tokens, program) = crate::parse::new::parse_expression("4d6k3 + 2".as_bytes()).unwrap().1;
    for_! { (term, ancestors) in program.postorder() => {
        dbg!(term);
        for term in ancestors {
            println!("Ancestor: {:?}", term);
        }
    }}
}

#[cfg(test)]
#[test]
fn postorder() {
    macro_rules! decl_consts {
        ($arena:ident => {
            $($name:ident($val:expr)),* $(,)?
        }) => {
            $(let $name: Id<Term> = $arena.alloc(Term::Constant($val));)*
        }
    }
    macro_rules! decl_adds {
        ($arena:ident => {
            $($name:ident($first:expr, $second:expr)),* $(,)?
        }) => {
            $(let $name: Id<Term> = $arena.alloc(Term::Add($first, $second));)*
        }
    }
    let mut arena = Arena::<_, id_arena::DefaultArenaBehavior<Term>>::new();
    decl_consts!(arena => {
        a(10),
        b(11),
        c(12),
        d(13),
    });
    decl_adds!(arena => {
        first(a, b),
        second(c, d),
        third(first, second),
    });
    let tree = Tree {
        arena, top: third,
    };
    let mut iter_walker_output = Vec::new();
    for_! { (term, _ancestors) in tree.postorder() => {
        iter_walker_output.push(term.clone());
    }}
    let proggy = crate::parse::new::Program { tree };
    let mut recursive_walker_output = Vec::new();
    crate::stack::postorder(&proggy, |term, _parent| {
        recursive_walker_output.push(term.clone());
    });
    assert_eq!(iter_walker_output, recursive_walker_output);
}
