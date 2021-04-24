//! Generate GraphViz DOT files for ASTs.
use crate::parse::new::{Program, Term};
use ::id_arena::{Arena, Id as ArenaId};

struct IdGen {
    count: u32,
}
struct Id(u32);
impl IdGen {
    const fn new() -> Self {
        Self { count: 0 }
    }
    fn next(&mut self) -> Id {
        let count = self.count;
        self.count = self.count.checked_add(1).expect("node count shouldn't overflow in any practical program");
        Id(count)
    }
}
impl Id {
    fn fmt(&self, buf: &mut String) {
        buf.push_str("N");
        ::itoa::fmt(buf, self.0).unwrap();
    }
}

/// Generate a GraphViz DOT file.
pub fn make_dot(program: &Program) -> String {
    let mut graph = String::from("strict digraph {\n");
    let mut gen = IdGen::new();
    let Program { terms, top } = program;
    
    fn write_dot(graph: &mut String, gen: &mut IdGen, terms: &Arena<Term>, term: &Term) -> Id {
        let push_node = |graph: &mut String, id: &Id, label: String| {
            graph.push_str("\t");
            id.fmt(graph);
            graph.push_str(" [label = \"");
            graph.push_str(&label);
            graph.push_str("\"]\n");
        };
        // Takes two node ids
        let push_edge = |graph: &mut String, a: &Id, b: &Id| {
            graph.push_str("\t");
            a.fmt(graph);
            graph.push_str(" -> ");
            b.fmt(graph);
            graph.push_str("\n");
        };
        // TODO: consider generalizing to n-ary operators
        let write_op = |graph: &mut String, gen: &mut IdGen, root: String, left: &ArenaId<Term>, right: Option<&ArenaId<Term>>| -> Id {
            let id = gen.next();
            push_node(graph, &id, root);
            let left_id = write_dot(graph, gen, terms, &terms[*left]);
            push_edge(graph, &id, &left_id);

            if let Some(right) = right {
                let right_id = write_dot(graph, gen, terms, &terms[*right]);
                push_edge(graph, &id, &right_id);
            }
            id
        };
        match term {
            Term::Constant(n) => {
                let id = gen.next();
                push_node(graph, &id, format!("{}", n));
                id
            },
            Term::DiceRoll(count, sides) => {
                let id = gen.next();
                let left_id = gen.next();
                let right_id = gen.next();
                push_node(graph, &id, String::from("d"));
                push_node(graph, &left_id, format!("{}", count));
                push_node(graph, &right_id, format!("{}", sides));
                push_edge(graph, &id, &left_id);
                push_edge(graph, &id, &right_id);
                id
            },
            Term::Add(left, right) => write_op(graph, gen, String::from("+"), left, Some(right)),
            Term::Subtract(left, right) => write_op(graph, gen, String::from("-"), left, Some(right)),
            Term::UnarySubtract(only) => write_op(graph, gen, String::from("-"), only, None),
            Term::UnaryAdd(only) => write_op(graph, gen, String::from("+"), only, None),
        }
    }
    write_dot(&mut graph, &mut gen, terms, &terms[*top]);
    graph.push_str("}\n");
    graph
}
