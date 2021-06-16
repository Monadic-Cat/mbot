//! Types and parsers for dice expressions.

/// A new parsing module, in my continuing effort to rework this entire thing.
/// Eventually, this will be the only parsing module in `mice`.
pub mod new {
    // A new parsing module, in my continuing effort to rework this entire thing.
    // Note that all byte strings in this module are conventionally UTF-8.
    // TODO: consider using the `bstr` crate
    use ::id_arena::{Arena, Id};
    use crate::tree::Tree;
    use ::core_extensions::SliceExt;
    use ::core::convert::TryFrom;
    use ::core::convert::TryInto;

    use ::proc_macro_helpers::decl_ops;
    decl_ops! {
        // TODO: if Token::D becomes an operator,
        // we may wish to encode whitespace sensitivity here,
        // so we can continue requiring the absence of whitespace
        // inside dice terms.
        #[non_exhaustive]
        #[derive(Debug, Clone, Copy)]
        pub enum
        /// Operators
            Op =
        /// Unary operators
            UnaryOp
        /// Binary operators
            BinOp {
                /// Addition
                Plus { unary: 3, binary: (3, 4) },
                /// Subtraction
                Minus { unary: 3, binary: (3, 4) },
                // /// Multiplication
                // Times { binary: (5, 6) },
                // In order to have general operators that work on
                // non integers (like 'k' in '4d6k3'), we're gonna need
                // a type checker. Some restricted forms of these don't
                // require that, but might as well point it out.
            }
        unary =>
        /// A description of how tightly unary operators bind their arguments.
        fn unary_binding_power(op) -> u8;
        binary =>
        /// A description of how tightly infix operators bind their arguments.
        /// This is how we handle precedence.
        fn infix_binding_power(op) -> (u8, u8);
    }

    /// Units of input as segmented by the lexer.
    #[derive(Debug)]
    pub enum Token {
        /// This will never be negative. We use an [`i64`] here so numeric limits line up elsewhere.
        Int(i64),
        D,
        K,
        Op(Op),
        Whitespace,
    }

    /// Parse error for an integer that's too large for our number type.
    #[derive(Debug)]
    struct TooLarge;
    /// Lex a dice expression. Returns a slice reference to trailing unlexed input.
    fn lex(input: &[u8]) -> (&[u8], Result<Vec<Token>, TooLarge>) {
        enum State<'a> {
            Normal,
            Int(&'a [u8]),
            // We don't care about whitespace, at least for now.
            // This just throws it out.
            Whitespace,
        }
        let mut tokens = Vec::with_capacity(input.len());
        let mut state = State::Normal;
        let mut cursor = input;
        loop {
            match state {
                State::Normal => match cursor {
                    [b'0'..=b'9', rest @ ..] => {
                        state = State::Int(cursor);
                        cursor = rest;
                    },
                    [b'd', rest @ ..] => {
                        tokens.push(Token::D);
                        cursor = rest;
                    },
                    [b'k', rest @ ..] => {
                        tokens.push(Token::K);
                        cursor = rest;
                    },
                    [b'+', rest @ ..] => {
                        tokens.push(Token::Op(Op::Plus));
                        cursor = rest;
                    },
                    [b'-', rest @ ..] => {
                        tokens.push(Token::Op(Op::Minus));
                        cursor = rest;
                    },
                    [b'\t', rest @ ..] | [b' ', rest @ ..] => {
                        tokens.push(Token::Whitespace);
                        state = State::Whitespace;
                        cursor = rest;
                    },
                    [_, _rest @ ..] => break,
                    [] => break,
                },
                State::Int(start) => match cursor {
                    [b'0'..=b'9', rest @ ..] => cursor = rest,
                    [..] => {
                        let slice = &start[..start.offset_of_slice(cursor)];
                        use ::checked::Checked;
                        tokens.push(Token::Int(match slice.iter().try_fold(0i64, |a, b| {
                            *((Checked::from(a) * 10) + (b - b'0') as i64)
                        }) {
                            Some(x) => x,
                            None => return (cursor, Err(TooLarge)),
                        }));
                        state = State::Normal;
                        // Note that we're specifically choosing not to advance the cursor here.
                    }
                },
                State::Whitespace => match cursor {
                    [b'\t', rest @ ..] | [b' ', rest @ ..] => cursor = rest,
                    [..] => state = State::Normal,
                }
            }
        }
        (cursor, Ok(tokens))
    }

    /// An AST node. This represents an expression as a tree of nodes stored in an [`Arena`].
    #[non_exhaustive]
    #[derive(Clone, Debug, ::derive_more::Unwrap, PartialEq)]
    pub enum Term {
        Constant(i64),
        // This could conceivably have its arguments
        // replaced by terms, and be turned into an operator
        // in its own right. This could then allow strange expressions like `3d(d8)`.
        DiceRoll(i64, i64),
        KeepHigh(Id<Term>, i64),
        Add(Id<Term>, Id<Term>),
        Subtract(Id<Term>, Id<Term>),
        UnarySubtract(Id<Term>),
        UnaryAdd(Id<Term>),
    }

    /// A parsed dice program. The result of invoking `parse_expression` on something like `"3d6 + 4"`.
    // Fuck it. Dice expressions are programs.
    // Note: I fully intend to expose the AST as public API.
    #[derive(Debug)]
    pub struct Program {
        // Note that `Program`s are intended to be correctly formed by construction.

        // We allocate terms inside an arena so we can build trees without
        // allocating for each node.
        pub(crate) tree: Tree<Term>,
    }
    impl ::core::ops::Deref for Program {
        type Target = Tree<Term>;
        fn deref(&self) -> &Self::Target {
            &self.tree
        }
    }

    /// For debugging purposes.
    /// This writes out the parse tree starting from `top` as an S-expression.
    fn write_sexpr(terms: &Arena<Term>, top: Id<Term>, buf: &mut String) {
        let mut write_op = |op: &str, lhs, rhs| {
            buf.push('(');
            buf.push_str(op);
            buf.push(' ');
            write_sexpr(terms, lhs, &mut *buf);
            buf.push(' ');
            write_sexpr(terms, rhs, &mut *buf);
            buf.push(')');
        };
        match terms[top] {
            Term::Constant(n) => { itoa::fmt(&mut *buf, n).unwrap(); },
            Term::DiceRoll(count, faces) => {
                itoa::fmt(&mut *buf, count).unwrap();
                buf.push('d');
                itoa::fmt(&mut *buf, faces).unwrap();
            }
            Term::KeepHigh(roll, count) => {
                buf.push('(');
                buf.push('k');
                buf.push(' ');
                write_sexpr(terms, roll, &mut *buf);
                buf.push(' ');
                itoa::fmt(&mut *buf, count).unwrap();
                buf.push(')');
            },
            Term::Add(lhs, rhs) => write_op("+", lhs, rhs),
            Term::Subtract(lhs, rhs) => write_op("-", lhs, rhs),
            Term::UnaryAdd(arg) => {
                buf.push('+');
                write_sexpr(terms, arg, &mut *buf);
            }
            Term::UnarySubtract(arg) => {
                buf.push('-');
                write_sexpr(terms, arg, &mut *buf);
            }
        }
    }

    impl Program {
        /// For debugging purposes.
        /// This writes out the parse tree of a program as an S-expression.
        pub fn fmt_sexpr(&self) -> String {
            let mut buf = String::new();
            write_sexpr(&self.arena, self.top, &mut buf);
            buf
        }
        pub fn terms(&self) -> &Arena<Term> {
            &self.arena
        }
        pub fn is_single(&self) -> bool {
            let mut count = 0;
            crate::tree::for_! { (term, _) in self.postorder() => {
                match term {
                    Term::DiceRoll(dice_count, _sides) => count += dice_count,
                    _ => count += 1,
                }
            }}
            count == 1
        }
    }

    /// The return type of a parser function that returns trailing unparsed input
    /// on both success and failure.
    type ParseResult<I, O, E> = Result<(I, O), (I, E)>;


    // TODO: consider reporting what the token was
    /// An invalid token was found in expression position.
    struct InvalidTokenInExpr;
    /// Reached end of token stream while in expression position.
    struct UnexpectedEof;

    /// Expression parsing error.
    #[derive(Debug)]
    pub enum ExprError {
        // TODO: consider splitting lexing errors out
        // TODO: consider exposing the lexer separate from the parser as public API
        /// A parsed integer was too large for `u64`.
        TooLarge,
        /// Encountered invalid token in expression position.
        InvalidTokenInExpr,
        /// `d0`s are not valid dice.
        InvalidDie,
        /// Reached end of token stream while in expression position.
        // (Note that it isn't an error to encounter EOF in binary operator position.)
        Eof,
        /// Encountered a non binary operator in binary operator position.
        InvalidBinOp,
        /// Encountered an invalid token in binary operator position.
        InvalidTokenInBinOp,
        /// Encountered an invalid token in unary operator position.
        InvalidTokenInUnaryOp,
    }
    impl From<InvalidTokenInExpr> for ExprError {
        fn from(InvalidTokenInExpr: InvalidTokenInExpr) -> Self {
            Self::InvalidTokenInExpr
        }
    }
    impl From<UnexpectedEof> for ExprError {
        fn from(UnexpectedEof: UnexpectedEof) -> Self {
            Self::Eof
        }
    }

    /// Dice program parser combinator.
    /// Consumes input until it reaches unrecognizable tokens,
    /// and attempts to build a dice program from the consumed input.
    pub fn parse_expression(input: &[u8]) -> ParseResult<&[u8], (Vec<Token>, Program), ExprError> {
        let mut arena = Arena::<Term>::new();
        let (rest, tokens) = lex(input);
        let tokens = match tokens {
            Ok(x) => x,
            Err(TooLarge) => return Err((rest, ExprError::TooLarge)),
        };
        // To be used where we already know to expect a unary op.
        fn consume_unary_op(terms: &mut Arena<Term>, op: UnaryOp, input: &[Token]) -> Result<Id<Term>, ExprError>  {
            match op {
                UnaryOp::Plus => {
                    let term = Term::UnaryAdd(consume_expr(&mut *terms, unary_binding_power(op), input)?.1);
                    Ok(terms.alloc(term))
                },
                UnaryOp::Minus => {
                    let term = Term::UnarySubtract(consume_expr(&mut *terms, unary_binding_power(op), input)?.1);
                    Ok(terms.alloc(term))
                }
            }
        }

        /// Scroll past whitespace.
        fn ignore_whitespace(input: &[Token]) -> &[Token] {
            let mut cursor = input;
            while let [Token::Whitespace, rest @ ..] = cursor {
                cursor = rest;
            }
            cursor
        }

        fn consume_expr<'a>(terms: &mut Arena<Term>, min_bp: u8, input: &'a [Token])
                        -> Result<(&'a [Token], Id<Term>), ExprError> {
            // Ideally we'd enforce this check via a constrained constructor
            // for dice terms, but I'm doing a quick fix lol.
            macro_rules! check_faces {
                ($faces:expr) => {
                    if $faces <= 0 {
                        return Err(ExprError::InvalidDie)
                    }
                }
            }
            let (mut cursor, mut lhs) = match ignore_whitespace(input) {
                // Currently we parse a dice term like a terminal, but
                // there is no reason we couldn't make `d` into an operator as well.
                // That said, dice terms are liable to become much more complicated.
                // For now, the extra flexibility that would come from that is
                // not necessary or wanted.
                [Token::Int(count), Token::D, Token::Int(faces), Token::K, Token::Int(keep_count), rest @ ..] => {
                    check_faces!(*faces);
                    let roll = Term::DiceRoll(*count, *faces);
                    let keep_high = Term::KeepHigh(terms.alloc(roll), *keep_count);
                    (rest, keep_high)
                },
                [Token::D, Token::Int(faces), Token::K, Token::Int(keep_count), rest @ ..] => {
                    check_faces!(*faces);
                    let roll = Term::DiceRoll(1, *faces);
                    let keep_high = Term::KeepHigh(terms.alloc(roll), *keep_count);
                    (rest, keep_high)
                },
                [Token::Int(count), Token::D, Token::Int(faces), rest @ ..] => {
                    check_faces!(*faces);
                    (rest, Term::DiceRoll(*count, *faces))
                },
                [Token::D, Token::Int(faces), rest @ ..] => {
                    check_faces!(*faces);
                    (rest, Term::DiceRoll(1, *faces))
                },
                [Token::Int(n), rest @ ..] => (rest, Term::Constant(*n)),
                [_x, ..] => Err(InvalidTokenInExpr)?,
                [] => Err(UnexpectedEof)?,
            };

            loop {
                let (rest, op) = match cursor {
                    [Token::Op(op), rest @ ..] => (rest, BinOp::try_from(*op).map_err(|()| {
                        ExprError::InvalidBinOp
                    })?),
                    [Token::Whitespace, rest @ ..] => {
                        cursor = rest;
                        continue
                    },
                    [_x, ..] => Err(ExprError::InvalidTokenInBinOp)?,
                    [] => break,
                };
                let (l_bp, r_bp) = infix_binding_power(op);
                if l_bp < min_bp {
                    break
                }

                cursor = rest;
                let (rest, rhs) = consume_expr(&mut *terms, r_bp, cursor)?;
                cursor = rest;
                match op {
                    BinOp::Plus => lhs = Term::Add(terms.alloc(lhs), rhs),
                    BinOp::Minus => lhs = Term::Subtract(terms.alloc(lhs), rhs),
                }
            }
            Ok((cursor, terms.alloc(lhs)))
        }

        let mut cursor = &tokens[..];
        let result = loop {
            match cursor {
                // Ignore preceding whitespace.
                [Token::Whitespace, rest @ ..] => cursor = rest,
                // Note that unary operations are currently only permitted at the
                // front of a dice expression. We could be more permissive than this,
                // but the current goal is identical behavior to the old parser.
                [Token::Op(op), rest @ ..] => break match (*op).try_into() {
                    Ok(op) => consume_unary_op(&mut arena, op, rest),
                    Err(()) => Err(ExprError::InvalidTokenInUnaryOp),
                },
                [Token::K, ..] => break Err(ExprError::InvalidTokenInUnaryOp),
                all @ [Token::Int(_), ..] |
                all @ [Token::D, ..] => break consume_expr(&mut arena, 2, all).map(|(_, x)| x),
                [] => break Err(ExprError::Eof),
            };
        };
        match result {
            Ok(top) => Ok((rest, (tokens, Program { tree: Tree { arena, top }}))),
            Err(e) => Err((rest, e)),
        }
    }
}
