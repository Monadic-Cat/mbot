//! An IR language to lower dice programs to, with the goal of permitting more complex dice operators.
//!
//! Design goals include:
//! - Providing a good backend for end-user custom logic
//! - Supporting a slow runtime mode that keeps many intermediate results
//!   for the purpose of end-user visible textual output
//! - Supporting a fast runtime mode for statistical sampling and analysis
//! 
//! These are both currently implemented in the [`interp`](crate::interp) and
//! [`stack`](crate::stack) modules, and supporting these use cases is a
//! requirement for a shift to this IR.
//! 
//! And we have possible stretch goals of:
//! - Supporting a possibly slow*er* runtime mode that keeps *all* intermediate
//!   results for the purpose of end-user visible 3D dice roll animations
//!   (this one's outside of my use case, but this IR shouldn't stand in the way of doing that)
//! - Supporting a JIT compiled runtime mode for *faster* statistical sampling and analysis
//! - Supporting a GPU based backend for *even faster* statistical sampling and analysis
use ::id_arena::{Id, Arena};
use crate::parse::Term;

struct DiceRoll {
    count: i64,
    sides: i64,
}

struct FateDie;
impl FateDie {
    fn roll<R: ::rand::Rng>(rng: &mut R) -> Result<i64, ::core::convert::Infallible> {
        Ok(rng.gen_range(0, 3) - 1)
    }
}

// I need a MIR abstraction for dice roll.
// The input dice language will have some coercions,
// but those should ideally be made explicit during lowering.
// There should be some minor type inference on operators, but
// since all input data is given in literals, it should be fairly simple.
// So, MIR should be fully typed, and then type checked.

// List of coercions in the input dice language:
//   DiceRoll -> DiceResult
//   DiceResult -> i64
// Coercions `a -> b` occur when an operator that accepts `b`, but not `a`, is given
// an argument of type `a`.

#[derive(Debug)]
struct DiceRollOutput {
    sides: i64,
    dice: Vec<i64>,
}

enum Sign {
    /// Positive filter: Choosing what to keep.
    Positive,
    /// Negative filter: Choosing what to drop.
    Negative,
}

struct DieId {
    idx: usize,
}

// When the maximum depth of a dice program's output is 1,
// phases will not be drawn.
/// A layer of evaluation.
/// These are created by operators that work on collections of dice.
struct Layer {
    /// Sign of the filter from which this layer was produced.
    sign: Sign,
    layer: Vec<DieId>,
    // TODO: keep pragmatic sign for reduced copies?
}

struct StackedOutput {
    // Keeping a fully applied copy of the dice output,
    // so we don't have to walk the entire layer stack for every die at every new layer.
    top: DiceRollOutput,
    // For extremely deep stacks, we should discard the intermediate layers.
    // That'll keep us from a really bad case of quadratic space.
    // Imagine (...((10000d10k10000)k10000)k10000)...)
    // Of course, such a thing can be simply denied via our cost estimation machinery.
    layers: Vec<Layer>,
    output: DiceRollOutput,
}

// Conceptually, a filter is a function:
//    DiceRollOutput -> DiceRollOutput
// Though we realistically need to keep track of the dice that are dropped as well.
// 
/// A unification of Keep High, Keep Low, Drop High, and Drop Low.
#[derive(Debug)]
struct Filter {
    filter: FilterKind,
}

#[derive(Debug)]
enum FilterKind {
    KeepHigh,
    KeepLow,
    DropHigh,
    DropLow,
}
impl FilterKind {
    // This will be used to determine whether we highlight what is kept, or
    // cross out what is removed.
    /// Whether the filter operates by choosing which dice to keep,
    /// or by choosing which dice to throw away.
    fn sign(&self) -> Sign {
        use FilterKind::*;
        match self {
            KeepHigh => Sign::Positive,
            KeepLow => Sign::Positive,
            DropHigh => Sign::Negative,
            DropLow => Sign::Negative,
        }
    }
}

/// Coercions, left implicit in the AST but explicit in MIR.
#[derive(Debug)]
enum Coercion {
    FromOutputToInt,
}

enum Type {
    Output,
    Integer,
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Subtract
}

/// A *region*, as in RVSDG.
#[derive(Debug)]
struct Region {
    graph: MirGraph,
    end: NodeIndex,
}

/// A dice program lowered to a graph structure based on RVSDG.
#[derive(Debug)]
pub struct Mir {
    graph: MirGraph,
    top: NodeIndex,
}

fn mir_type_of(graph: &MirGraph, node: NodeIndex) -> Type {
    match graph[node] {
        MirNode::Integer(_) => Type::Integer,
        MirNode::Coerce(Coercion::FromOutputToInt) => Type::Integer,
        MirNode::Roll => Type::Output,
        MirNode::BinOp(BinaryOp::Add | BinaryOp::Subtract) => Type::Integer,
        MirNode::Filter(_) => Type::Output,
        MirNode::Apply => todo!("type of function application"),
        MirNode::Loop(_) => todo!("type of loops"),
        MirNode::Lambda(_) => todo!("type of function declaration"),
        MirNode::Entry => todo!("type of entry point"),
        MirNode::End => todo!("type of region end")
    }
}

/// Attempt to coerce a node's result to a given type.
/// Succeeds by identity if the node is already of the type given.
fn mir_coerce_to(graph: &mut MirGraph, node: NodeIndex, ty: Type) -> Option<NodeIndex> {
    macro_rules! coerce {
        ($coercion:ident) => {{
            let coercion = graph.add_node(MirNode::Coerce(Coercion :: $coercion));
            graph.add_edge(coercion, node, MirEdge::DataDependency { port: 0 });
            coercion
        }}
    }
    match (ty, mir_type_of(&*graph, node)) {
        (Type::Output, Type::Output) => Some(node),
        // There is no way to coerce an integer back into a collection of dice output.
        (Type::Output, Type::Integer) => None,
        (Type::Integer, Type::Output) => Some(coerce!(FromOutputToInt)),
        (Type::Integer, Type::Integer) => Some(node),
    }
}


// Typechecking should occur at MIR construction time.
// In particular, only valid coercions should be inserted.
// 
// The MIR takes the form of an acyclic data dependency graph.
// At most, permit structured control flow via regions as in RVSDG.
// Ideally, we would only permit terminating programs to be constructed,
// but dice explosions make that tricky. However, there's no reason we couldn't
// encode evaluation fuel in the graph itself, right?
// That way guaranteed termination remains encoded in the program representation.
// Unfortunately, if we want to share evaluation fuel fairly across all explosions
// in an expression, the program representation cannot be a tree- it requires something
// like the state edges in RVSDG (which would probably be filled in left to right).
// Without a tree representation, making a recursive interpreter will be
// trickier, to say the least.

/// A node in the MIR data dependency graph.
#[derive(Debug)]
enum MirNode {
    Integer(i64),
    Coerce(Coercion),
    // TODO: generalize over dice roll kinds?
    Roll,
    BinOp(BinaryOp),
    Filter(Filter),
    Apply,
    // These contain *regions*, as in RVSDG.
    // TODO: Both of these need to have an explicit type attached.
    Loop(Region),
    Lambda(Region),
    Entry,
    End,
}

/// A dummy macro so I can write example snippets with full editor support.
macro_rules! mir {
    ($($t:tt)*) => {}
}
mir! {
    // An example of how we might structure an explosion in MIR.
    let mut results = Stack::new();
    let mut to_roll = (2, 6);
    loop {
        let result = roll(..to_roll);
        results.push(result);
        to_roll = (count((== 6), result), to_roll.1);
    } while (to_roll.0 != 0);
}

/// An edge in the MIR data dependency graph.
#[derive(Debug)]
enum MirEdge {
    DataDependency { port: u8 },
    FuelDependency,
}

use ::petgraph::{Graph, Directed, graph::NodeIndex};
type MirGraph = Graph<MirNode, MirEdge, Directed>;

/// Lower a parsed dice program to MIR.
/// Currently infallible, as the parser already rejects ill-typed programs.
pub fn lower(ast: &crate::parse::Program) -> Mir {
    let mut graph = MirGraph::new();
    // Evaluation order is depth first, left to right, across the
    // whole dice program.
    // There may be optimization opportunities that involve relaxing
    // that ordering, of varying quality.
    // First, rolls are held to be independent of each other-
    // we do not guarantee a particular starting seed in any context,
    // although it has been considered for reproducible sample distribution
    // plotting.
    // Therefore, we can reorder rolls, or perform them in parallel, if it would
    // improve performance.
    // Second, we may completely fold all constant arithmetic, even though doing so
    // may change the overflow behavior near integer limits, as I have made
    // no particular guarantee around overflow handling.
    // It would also be possible to only fold constant arithmetic that does not overflow,
    // or which would definitely overflow at runtime *anyway*.
    /// State kept while scrolling depth first, left to right, across
    /// the dice program we're lowering to MIR.
    struct State {
        // Note that `Graph` indices are stable as long as you don't
        // *remove* any nodes or edges. Since we don't do that during
        // MIR construction, we don't need to worry about this.
        last_fuel_use: Option<NodeIndex>,
    }
    impl State {
        fn new() -> Self {
            Self { last_fuel_use: None, }
        }
        /// Add a runtime fuel dependency. This is for operations whose fuel use cannot
        /// be known statically, prior to runtime.
        #[allow(dead_code)]
        fn add_fuel_use(&mut self, graph: &mut MirGraph, node: NodeIndex) {
            if let Some(last_fuel_use) = self.last_fuel_use {
                graph.add_edge(node, last_fuel_use, MirEdge::FuelDependency);
            }
            self.last_fuel_use = Some(node);
        }
    }
    let mut state = State::new();
    fn lower_node(terms: &Arena<Term>,
                  graph: &mut MirGraph,
                  state: &mut State,
                  current: Id<Term>) -> NodeIndex {
        fn coerce_to_int(graph: &mut MirGraph, term: NodeIndex) -> NodeIndex {
            match mir_type_of(&*graph, term) {
                Type::Output => {
                    let coercion = graph.add_node(MirNode::Coerce(Coercion::FromOutputToInt));
                    graph.add_edge(coercion, term, MirEdge::DataDependency { port: 0 });
                    coercion
                },
                Type::Integer => term,
            }
        }
        // TODO: Consider macro generating more of this code.
        match &terms[current] {
            Term::Constant(val) => graph.add_node(MirNode::Integer(*val)),
            Term::DiceRoll(count, sides) => {
                let count = graph.add_node(MirNode::Integer(*count));
                let sides = graph.add_node(MirNode::Integer(*sides));
                let roll = graph.add_node(MirNode::Roll);
                graph.add_edge(roll, count, MirEdge::DataDependency { port: 0 });
                graph.add_edge(roll, sides, MirEdge::DataDependency { port: 1 });
                roll
            },
            Term::KeepHigh(term, keep_count) => {
                // At least for now, the parser doesn't permit other things in here.
                assert!(matches!(&terms[*term], Term::DiceRoll(_, _)));
                let roll = lower_node(terms, &mut *graph, &mut *state, *term);
                let keep_count = graph.add_node(MirNode::Integer(*keep_count));
                let keep_high = graph.add_node(MirNode::Filter(Filter {
                    filter: FilterKind::KeepHigh,
                }));
                graph.add_edge(keep_high, roll, MirEdge::DataDependency { port: 0 });
                graph.add_edge(keep_high, keep_count, MirEdge::DataDependency { port: 1 });
                keep_high
            },
            Term::Add(left, right) | Term::Subtract(left, right) => {
                let left = lower_node(terms, &mut *graph, &mut *state, *left);
                let right = lower_node(terms, &mut *graph, &mut *state, *right);
                let left = coerce_to_int(&mut *graph, left);
                let right = coerce_to_int(&mut *graph, right);
                let total = match &terms[current] {
                    Term::Add(_, _) => BinaryOp::Add,
                    Term::Subtract(_, _) => BinaryOp::Subtract,
                    _ => unreachable!()
                };
                let total = graph.add_node(MirNode::BinOp(total));
                graph.add_edge(total, left, MirEdge::DataDependency { port: 0 });
                graph.add_edge(total, right, MirEdge::DataDependency { port: 1 });
                total
            },
            // TODO: decide if we want a distinct negation operator.
            // Currently we just lower `-x` to `0 - x`.
            Term::UnarySubtract(term) => {
                let zero = graph.add_node(MirNode::Integer(0));
                let term = lower_node(terms, &mut *graph, &mut *state, *term);
                let term = coerce_to_int(&mut *graph, term);
                let total = graph.add_node(MirNode::BinOp(BinaryOp::Subtract));
                graph.add_edge(total, zero, MirEdge::DataDependency { port: 0 });
                graph.add_edge(total, term, MirEdge::DataDependency { port: 1 });
                total
            },
            Term::UnaryAdd(term) => {
                // TODO: insert integer coercion?
                // As `+x` is equivalent to `x`, we just elide this operation
                // when lowering to MIR.
                let term = lower_node(terms, &mut *graph, &mut *state, *term);
                term
            }
        }
    }
    let top = lower_node(ast.terms(), &mut graph, &mut state, ast.top);
    Mir { graph, top }
}

pub fn dot(mir: &Mir) -> String {
    format!("{:?}", ::petgraph::dot::Dot::new(&mir.graph))
}

macro_rules! defops {
    ($(
        $(#[$attr:meta])*
        $fname:ident [$($imparg:ident $impty:ty),*]($($arg:ident $arty:ty),*) -> $rety:ty {
            $($t:tt)*
        }
    )*) => {
        
    }
}

/// A basic interpreter for the MIR.
/// Not usable yet.
pub mod interp {
    use super::{Mir, MirNode, MirEdge, Coercion, DiceRollOutput, BinaryOp, Filter, FilterKind};
    use ::petgraph::{Direction, visit::DfsPostOrder};
    use ::rand::Rng;
    use ::std::cmp;

    #[derive(::derive_more::Unwrap, Debug)]
    enum Value {
        Integer(i64),
        Output(i64, DiceRollOutput),
    }

    #[derive(Debug)]
    struct StackVar {
        port: u8,
        value: Value,
    }

    // This is non-optional because MIR construction and checking should move all
    // possible errors to prior to interpretation.
    // So therefore, any failures are due to programming mistakes.
    fn pop_arguments<const N: usize>(stack: &mut Vec<StackVar>) -> [Value; N] {
        use ::core::mem::{MaybeUninit, transmute_copy};
        const MAYBE_VALUE: MaybeUninit<Value> = MaybeUninit::uninit();
        let mut buf = [MAYBE_VALUE; N];
        // We only keep track of initialization here so we don't have ambient unsafety.
        // If MIR is constructed and verified right and we make a
        // mistake in the interpreter, this will never be necessary.
        let mut inited = [false; N];
        for _ in 0..N {
            // TODO: consider moving all popping to the front by instead using
            // drain or split_off.
            let StackVar { port, value } = stack.pop().unwrap();
            assert!(!inited[port as usize]);
            inited[port as usize] = true;
            // Safety: Writing through a raw pointer immediately after
            // obtaining it from MaybeUninit is perfectly safe.
            unsafe { buf[port as usize].as_mut_ptr().write(value); }
        }
        // Safety: We have ensured the whole buffer is initialized.
        // Since either stack.pop().unwrap() or `assert!(!inited[port as usize])`
        // would otherwise have panicked, due to insufficient arguments or overlapping ports,
        // respectively.
        unsafe { transmute_copy::<[MaybeUninit<Value>; N], [Value; N]>(&buf) }
    }

    pub fn interpret(mir: &Mir) -> i64 {
        let mut postorder = DfsPostOrder::new(&mir.graph, mir.top);
        let mut priors: Vec<StackVar> = Vec::with_capacity(2);
        let mut rng = ::rand::thread_rng();
        // TODO: we rely on MIR being tree-like for data dependencies,
        // for discovering argument port numbers.
        // This is due to DfsPostOrder not giving us quite enough information.
        // Fortunately, MIR *is* tree-like.
        // It won't be forever, though, if/when we add common subexpression elimination.
        // It *will* always be acyclic, at least.
        let get_self_arg_number = |node| {
            if mir.graph.edges_directed(node, Direction::Incoming).count() == 0 {
                // In this case, we're looking at the entry node.
                0
            } else {
                mir.graph.edges_directed(node, Direction::Incoming).filter_map(|e| {
                    match e.weight() {
                        &MirEdge::DataDependency { port } => Some(port),
                        _ => None
                    }
                }).next().unwrap()
            }
        };
        while let Some(node) = postorder.next(&mir.graph) {
            match &mir.graph[node] {
                &MirNode::Integer(val) => {
                    priors.push(StackVar {
                        port: get_self_arg_number(node),
                        value: Value::Integer(val)
                    });
                },
                MirNode::Coerce(Coercion::FromOutputToInt) => {
                    let (total, _output) = priors.pop().unwrap().value.unwrap_output();
                    priors.push(StackVar {
                        port: get_self_arg_number(node),
                        value: Value::Integer(total)
                    });
                },
                MirNode::Roll => {
                    let [count, sides] = pop_arguments(&mut priors);
                    let count = count.unwrap_integer();
                    let sides = sides.unwrap_integer();
                    if sides == 1 {
                        priors.push(StackVar {
                            port: get_self_arg_number(node),
                            value: Value::Output(count, DiceRollOutput {
                                sides, dice: Vec::new(),
                            })
                        });
                    } else {
                        let mut total: i64 = 0;
                        let mut parts = Vec::with_capacity(count as usize);
                        for _ in 0..count {
                            let random = rng.gen_range(0, sides) + 1;
                            total = total.checked_add(random).unwrap();
                            parts.push(random);
                        }
                        priors.push(StackVar {
                            port: get_self_arg_number(node),
                            value: Value::Output(total, DiceRollOutput {
                                sides, dice: parts
                            })
                        });
                    }
                },
                MirNode::BinOp(BinaryOp::Add) => {
                    let [left, right] = pop_arguments(&mut priors);
                    let left = left.unwrap_integer();
                    let right = right.unwrap_integer();
                    priors.push(StackVar {
                        port: get_self_arg_number(node),
                        value: Value::Integer(left + right),
                    });
                },
                MirNode::BinOp(BinaryOp::Subtract) => {
                    let [left, right] = pop_arguments(&mut priors);
                    let left = left.unwrap_integer();
                    let right = right.unwrap_integer();
                    priors.push(StackVar {
                        port: get_self_arg_number(node),
                        value: Value::Integer(left - right)
                    });
                },
                MirNode::Filter(Filter { filter: FilterKind::KeepHigh }) => {
                    let [output, keep_count] = pop_arguments(&mut priors);
                    let (total, output) = output.unwrap_output();
                    let keep_count = keep_count.unwrap_integer();
                    if output.sides == 1 {
                        priors.push(StackVar {
                            port: get_self_arg_number(node),
                            value: Value::Output(cmp::min(keep_count, total), DiceRollOutput {
                                sides: 1,
                                dice: Vec::new(),
                            })
                        });
                    } else {
                        let mut dice = output.dice;
                        dice.sort_unstable_by(|a, b| b.cmp(a));
                        dice.truncate(keep_count as usize);
                        let total = dice.iter().sum();
                        priors.push(StackVar {
                            port: get_self_arg_number(node),
                            value: Value::Output(total, DiceRollOutput {
                                sides: output.sides,
                                dice,
                            })
                        });
                    }
                }
                _ => todo!()
            }
        }
        dbg!(&priors);
        todo!()
    }
}

// A sketch of a macro DSL for declaring operators.
// Gonna go down a list of common dice operators and ensure most of them are expressible thusly.
// We may be able to generate (at least parts of) both the parser and interpreters from this.
// Square brackets are for implicit arguments.
defops! {
    // TODO: sort out appropriate operator binding powers
    #[magic("roll-intrinsic")]
    #[infix("d", 7, 8)]
    // TODO: figure out how to make this polymorphic over
    // kind of sides, as Fate dice notation `4dF` is not
    // able to be coerced to regular side counts nicely.
    roll[](count i64, sides i64) -> RollOut<count> {
        if sides == 1 {
            // The fact that rolling doesn't construct a list
            // for 1 sided dice is an optimization that other
            // operators shouldn't need to know about or deal with.
            RollOut(count, None)
        } else {
            let out = (0..count).fold(0, |a, _| a + gen_range(0, sides) + 1).collect()?;
            RollOut(out.sum(), out)
        }
    }

    // All operators should be bounds checked, but they are allowed to terminate the interpreter.
    // So, we can leave that checking implicit here, and simply substitute appropriate types.
    // Essentially on this level, InterpError is an unhandleable exception.
    // Though, ideally, operator bodies should be pure Rust.
    // (Albeit with a bunch of implicitly imported functions, and traits to ease coercions.)
    // That would reduce the amount of implementation work.
    // Runtime defined operators written in a scripting language are out of scope here.
    #[infix("+", 3, 4, allow_whitespace)]
    add[](a i64, b i64) -> i64 { a + b }
    #[infix("-", 3, 4, allow_whitespace)]
    subtract[](a i64, b i64) -> i64 { a - b }

    #[infix("k", 5, 6)]
    keep_high[n i64](rolls RollOut<n>, keep_count i64) -> Layer<min(n, keep_count), sign = Positive> {
        slice_lossy(0, keep_count, sorted(rolls))
    }
    #[infix("kl", 5, 6)]
    keep_low[n i64](rolls RollOut<n>, keep_count i64) -> Layer<min(n, keep_count), sign = Positive> {
        slice_lossy(0, keep_count, reverse(sorted(rolls)))
    }
    // Flip output layer sign.
    // #[infix("dh", 5, 6)]
    // drop_high = keep_high.flip();
    // #[infix("dl", 5, 6)]
    // drop_low = keep_low.flip();

    // Explosion takes an unevaluated roll as its parameter.
    // As explosions are potentially unbounded, we require they use
    // a finite supply of evaluation fuel.
    // (Evaluation fuel usage should be implicit for operators with guaranteed termination.)
    #[postfix("!", 5)]
    explode[fuel Fuel, n i64](roll Roll<n>) -> NonEmptyList<RollOut<n>> + Bottom<fuel> {
        // todo
    }
}
