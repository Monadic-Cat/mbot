use proc_macro::TokenStream;
use proc_macro::Ident;
use proc_macro::Span;
use proc_macro::TokenTree;
use proc_macro::Delimiter;
use proc_macro::Punct;
use proc_macro::Group;
use proc_macro::Literal;
use std::borrow::Cow;
use core::iter;

mod nfmt;

#[derive(Debug)]
struct TypeDeclaration {
    name: Ident,
    span: Span,
}

#[derive(Debug)]
struct ItemMeta {
    ty: TypeDeclaration,
    name: Ident,
}

#[derive(Debug)]
enum Meta {
    Item(Option<ItemMeta>),
    Expression,
}

#[derive(Debug)]
struct Spanned<T> {
    val: T,
    span: Span,
}
impl<T> Spanned<T> {
    fn new(span: Span, val: T) -> Self {
        Self { val, span }
    }
}
impl<T> quote::ToTokens for Spanned<T>
where T: quote::ToTokens {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Spanned { span, val } = self;
        let span = span.clone().into();
        tokens.extend(quote::quote_spanned!(span=> #val))
    }
}

#[derive(Debug)]
enum JoinSpecialVars {
    Expr,
    Sums,
    Total,
}

#[derive(Debug)]
enum MatchSpecialVars {
    Kind,
}

#[derive(Debug)]
enum Kind {
    Dice,
    Constant
}

#[derive(Debug)]
struct SpannedKind {
    kind: Kind,
    span: Span,
}

#[derive(Debug)]
struct MatchArm {
    span: Span,
}

#[derive(Debug)]
struct StringLiteral(Literal);

#[derive(Debug)]
enum Condition {
    Many,
}

#[allow(dead_code)]
#[derive(Debug)]
enum Instruction {
    InsertLiteralText(StringLiteral),
    Join(),
    Match(Vec<MatchArm>),
    If(Spanned<Condition>, Vec<Spanned<Instruction>>),
    Progn(Vec<Spanned<Instruction>>)
}
enum TermInstruction {
    // TODO: this
}

#[derive(Debug)]
struct Program {
    meta: Meta,
    top_calls: Vec<TopCall>,
    errors: Vec<SpannedError>,
}
impl Program {
    fn new() -> Self {
        Self {
            meta: Meta::Expression,
            top_calls: Vec::new(),
            errors: Vec::new(),
        }
    }
    fn into_stream(self) -> TokenStream {
        let mut output = TokenStream::new();
        match self.meta {
            Meta::Expression => {
                // Let's just collect all our errors into one big block expression.
                // Then, we can just cram it in wherever.
                let errors = TokenTree::Group(
                    Group::new(Delimiter::Brace,
                               self.errors.into_iter()
                               .map(|x| x.to_tokens()).collect()));
                // We'll wrap everything else in a block expression, too.
                let mut expression = TokenStream::new();
                expression.extend(iter::once(errors));
                let top_calls = self.top_calls;
                expression.extend(TokenStream::from(quote::quote! {
                    list![ #( #top_calls ),* ]
                }));
                let expression = TokenTree::Group(Group::new(Delimiter::Brace, expression));
                output.extend(iter::once(expression));
            },
            Meta::Item(_) => todo!("item position stuff")
        }
        println!("{}", output);
        output
    }
}

#[derive(Debug)]
enum TopCall {
    Const(ItemMeta),
    Join,
    If(Spanned<Condition>, Box<TopCall>),
    InsertLiteralText(StringLiteral),
    Progn(Vec<TopCall>),
}
enum FormatItemCdrTy<'a> {
    Nil,
    Progn(&'a [TopCall])
}
impl quote::ToTokens for FormatItemCdrTy<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // TODO: figure out if and how to attach spans to types we solve for
        match self {
            FormatItemCdrTy::Nil => tokens.extend(quote::quote!(Nil)),
            FormatItemCdrTy::Progn(_) => panic!("should have handled the progn case specially"),
        }
    }
}
impl TopCall {
    fn find_cdr_ty(&self) -> FormatItemCdrTy {
        match self {
            TopCall::Const(_) => {
                panic!("parsing should have issued an error on a const block in this position")
            },
            TopCall::Join => todo!("solving for Cdr type argument on join blocks"),
            TopCall::InsertLiteralText(_) => FormatItemCdrTy::Nil,
            TopCall::If(_, call) => call.find_cdr_ty(),
            TopCall::Progn(calls) => FormatItemCdrTy::Progn(calls),
        }
    }
}
impl quote::ToTokens for TopCall {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            TopCall::If(condition, call) => {
                match condition.val {
                    Condition::Many => {
                        let if_many_span = condition.span.into();
                        let cdr_ty = call.find_cdr_ty();
                        match cdr_ty {
                            FormatItemCdrTy::Nil => {
                                tokens.extend(quote::quote_spanned!(if_many_span=> IfMany::<'_, #cdr_ty, ()>));
                                tokens.extend(quote::quote! {
                                    {
                                        item: #call,
                                    }
                                })
                            },
                            FormatItemCdrTy::Progn(calls) => todo!("progn in `if` bodies")
                        }
                        
                    }
                }
            },
            TopCall::InsertLiteralText(text) => {
                let StringLiteral(literal) = text;
                let tree = proc_macro2::TokenStream::from(
                    TokenStream::from(TokenTree::Literal(literal.clone())));
                tokens.extend(quote::quote! {
                    FormatItem::Text(Cow::Borrowed(#tree))
                })
            }
            call => eprintln!("TODO: codegen for {:?}", call),
        }
    }
}

#[derive(Debug)]
struct SpannedError {
    span: Span,
    msg: Cow<'static, str>,
}
impl SpannedError {
    fn to_tokens(self) -> TokenStream {
        let SpannedError { span, msg } = self;
        let span = span.into();
        quote::quote_spanned!(span=> compile_error!(#msg);).into()
    }
}

struct Error {
    msg: Cow<'static, str>,
}
impl quote::ToTokens for Error {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Error { msg } = self;
        tokens.extend(quote::quote! { #msg })
    }
}

/// A replacement for `todo!` in macros that lets us apply spans.
macro_rules! todof {
    ($thing:literal in $span:expr) => {
        Err(SpannedError {
            msg: Cow::Borrowed(concat!("not yet implemented: ", $thing)),
            span: $span,
        })
    }
}

fn parse_top_level_group(group: &Group) -> Result<TopCall, SpannedError> {
    let input = group.stream().into_iter().collect::<Vec<_>>();
    let mut cursor = &input[..];
    match cursor {
        [TokenTree::Ident(ident), rest @ ..] => {
            let name = format!("{}", ident);
            match &*name {
                "const" => todof!("const blocks" in group.span()),
                "join" => todof!("join blocks" in group.span()),
                "if" => {
                    match rest {
                        [TokenTree::Ident(name), TokenTree::Punct(punct), rest @ ..] => {
                            if format!("{}", name) == "many" && punct.as_char() == '!' {
                                // TODO: join spans of name and punct
                                // Once Span::join is stabilized
                                let condition = Spanned::new(name.span(), Condition::Many);
                                match rest {
                                    [TokenTree::Group(if_true), TokenTree::Group(if_false)] => {
                                        todof!("if-else expressions" in group.span())
                                    },
                                    [TokenTree::Group(if_true)] => {
                                        let if_true = parse_top_level_group(if_true)?;
                                        Ok(TopCall::If(condition, Box::new(if_true)))
                                    },
                                    [TokenTree::Literal(literal)] => {
                                        let text = format!("{}", literal);
                                        if text.starts_with("\"") {
                                            Ok(TopCall::If(
                                                condition,
                                                Box::new(TopCall::InsertLiteralText(
                                                    StringLiteral(literal.clone())))))
                                        } else {
                                            Err(SpannedError {
                                                msg: Cow::Borrowed("expected string literal"),
                                                span: literal.span()
                                            })
                                        }
                                    }
                                    _ => Err(SpannedError {
                                        msg: Cow::Borrowed("invalid if expression"),
                                        span: group.span(),
                                    })
                                }
                            } else {
                                todof!("if conditions" in name.span())
                            }
                        },
                        [token, ..] => Err(SpannedError {
                            msg: Cow::Borrowed("unexpected stuff lol"),
                            span: token.span(),
                        }),
                        _ => Err(SpannedError {
                            msg: Cow::Borrowed("expected identifier, found end of group"),
                            span: group.span(),
                        }),
                    }
                },
                _ => Err(SpannedError {
                    span: ident.span(),
                    msg: Cow::Owned(format!("expected one of `const`, `join`, or `if`, found `{}`", name))
                }),
            }
        },
        [tree, ..] => Err(SpannedError {
            span: tree.span(),
            msg: Cow::Owned(format!("expected identifier, found {}", tree)), 
        }),
        _ => todo!()
    }
}

// TODO: make the parser error tolerant
#[proc_macro]
pub fn nfmt_prog(input: TokenStream) -> TokenStream {
    let input = input.into_iter().collect::<Vec<TokenTree>>();
    let mut cursor = &input[..];
    let mut prog = Program::new();
    while let [tree, ..] = cursor {
        println!("Next tree: {:#?}", tree);
        match tree {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                match parse_top_level_group(group) {
                    Ok(top_call) => prog.top_calls.push(top_call),
                    Err(e) => {
                        prog.errors.push(e);
                    },
                }
            },
            TokenTree::Group(group) => prog.errors.push(SpannedError {
                msg: Cow::Borrowed("only parentheses are valid as group delimiters"),
                // group.span_open() if/when that gets stabilized
                span: group.span(),
            }),
            // We permit semicolons wherever, so they can be used to massage
            // automatic formatters into indenting how we want.
            // Lookin' at you, Emacs.
            TokenTree::Punct(punct) if punct.as_char() == ';' => (),
            TokenTree::Punct(punct) => prog.errors.push(SpannedError {
                msg: Cow::Owned(format!("invalid punctuation: `{}`", punct)),
                span: punct.span(),
            }),
            token => prog.errors.push(SpannedError {
                msg: Cow::Owned(format!("unexpected token: {}", token)),
                span: token.span(),
            }),
        }
        cursor = &cursor[1..];
    }

    prog.into_stream()
}

#[proc_macro]
pub fn nfmt_prog_new(input: TokenStream) -> TokenStream {
    let program = nfmt::Program::parse(input.into());
    program.into_code().into()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
