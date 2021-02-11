use ::core::iter;
use ::std::borrow::Cow;
use ::proc_macro2::{TokenStream, Ident, Span, TokenTree, Delimiter, Punct, Group, Literal};

/// Generic wrapper for attaching [`Span`](proc_macro2::Span)s to types
// Rule of thumb for keeping spans:
// If a type contains more specific pieces, it should hold spans for those.
// But, it does not need to hold a span for the whole of itself.
// That span will be held by whatever holds values of that type.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Spanned<T> {
    pub(crate) span: Span,
    pub(crate) val: T,
}
impl<T> Spanned<T> {
    fn new(span: Span, val: T) -> Self {
        Self { span, val }
    }
}

// Note, our codegen backend requires string literals,
// but the runtime library is more flexible than that.
// This is because it is easier when everything
// in the output is `'static`.
// TODO: interpolations
// TODO: consider using a different representation for literals,
// which does not hold a redundant span.
#[derive(Debug, Clone)]
struct StringLiteral(Literal);

#[derive(Debug, Clone)]
enum TermSeparator {
    Operator,
    Comma,
    Text(StringLiteral),
}

#[derive(Debug, Clone, Copy)]
enum PartialSumSignDirective {
    Plus,
    Same,
}

#[derive(Debug, Clone, Copy)]
enum TermKind {
    Dice,
    Constant,
}

// This can lower directly to KindDependent,
// and it's much stronger than IfKind.
#[derive(Debug)]
struct MatchKind {
    dice_pattern: Span,
    constant_pattern:  Span,
    dice: Spanned<TermFormatItem>,
    constant: Spanned<TermFormatItem>,
}

#[derive(Debug)]
enum TermFormatItem {
    InsertTotal,
    InsertPartialSums(Spanned<PartialSumSignDirective>),
    InsertExpression,
    InsertSign,
    InsertText(StringLiteral),
    // Lowers to KindDependent.
    IfKind(Spanned<TermKind>, Box<Spanned<TermFormatItem>>),
    // Lowers to KindDependent.
    MatchKind(Box<MatchKind>),
    Nothing,
}

// GlobalPropertyDependent is open ended enough
// to support essentially anything we want.
// We'll probably have multiple more specific things
// that lower to it.
// We many end up unifying IfMany and IfNotMany, and possibly more.
#[derive(Debug)]
enum FormatItem {
    InsertTotal,
    InsertTerms(Spanned<TermSeparator>, Vec<Spanned<TermFormatItem>>),
    InsertText(StringLiteral),
    // Lowers to GlobalPropertyDependent.
    IfMany(Box<Spanned<FormatItem>>),
    // Lowers to GlobalPropertyDependent.
    IfNotMany(Box<Spanned<FormatItem>>),
    Nothing,
}

// -- Above this line is the very back of our codegen. -- \\
// It will be endowed with all the information needed to do codegen
// without solving, by construction. In particular, it will need to be
// given enough type information to insert type annotations where necessary.
// Next, is our higher level representation of NFMT programs.
// This is what our parser will construct.

#[derive(Debug, Clone, Copy)]
enum TopCondition {
    Many,
}

#[derive(Debug)]
enum TopCall {
    InsertTotal,
    Join(Spanned<TermSeparator>, Vec<Spanned<TermCall>>),
    InsertText(StringLiteral),
    If(Spanned<TopCondition>, Box<Spanned<TopCall>>, Box<Spanned<TopCall>>),
    Progn(Vec<Spanned<TopCall>>),
}

#[derive(Debug)]
struct TermMatch {
    dice_pattern: Span,
    constant_pattern: Span,
    dice: Vec<Spanned<TermCall>>,
    constant: Vec<Spanned<TermCall>>,
}

#[derive(Debug)]
enum TermCall {
    InsertTotal,
    InsertPartialSums(Spanned<PartialSumSignDirective>),
    InsertExpression,
    InsertText(StringLiteral),
    Match(Box<TermMatch>),
}

struct ItemCfg {
    name: Ident,
    type_name: Ident,
}

/// Program metadata.
struct Meta {
    // If this is present, the program should be placed in a constant item.
    item_cfg: Option<Spanned<ItemCfg>>,
}
impl Meta {
    fn new() -> Self {
        Self { item_cfg: None }
    }
}

#[derive(Debug)]
struct ParseErrorCollection {
    errors: Vec<Spanned<Cow<'static, str>>>,
}
impl ParseErrorCollection {
    fn new() -> Self {
        Self { errors: Vec::new() }
    }
    fn push(&mut self, error: Spanned<Cow<'static, str>>) {
        self.errors.push(error)
    }
    fn append(&mut self, errors: &mut ParseErrorCollection) {
        self.errors.append(&mut errors.errors);
    }
    fn iter(&self) -> impl Iterator<Item = &Spanned<Cow<'static, str>>> {
        self.errors.iter()
    }
    fn into_iter(self) -> impl Iterator<Item = Spanned<Cow<'static, str>>> {
        self.errors.into_iter()
    }
}

trait LiteralExt {
    fn is_string(&self) -> bool;
}
impl LiteralExt for Literal {
    fn is_string(&self) -> bool {
        // Note that neither `proc_macro` nor `proc_macro2` docs
        // state that the Display format for `Literal` is stable.
        // They do, however, state that the Display format for `TokenTree`
        // is stable. So, if this is not guaranteed, we can go through
        // `TokenTree`.
        matches!(format!("{}", self).as_bytes(), [b'"', ..])
    }
}
trait SpanExt {
    fn join_first(self, other: Span) -> Span;
}
impl SpanExt for Span {
    fn join_first(self, other: Span) -> Span {
        self.join(other).unwrap_or(self)
    }
}

#[cfg(test)]
#[test]
fn literal_ext_is_string() {
    let a = Literal::string("lol");
    let b = Literal::i32_unsuffixed(10);
    assert!(a.is_string());
    assert!(!b.is_string());
}

type ParseResult<I, O, E> = Result<(I, O), (I, E)>;

/// For reporting to the user what kind of expression
/// has been used wrong.
fn token_kind(tree: &TokenTree) -> &'static str {
    match tree {
        TokenTree::Group(_) => "call expression",
        TokenTree::Ident(_) => "identifier",
        TokenTree::Literal(_) => "literal",
        TokenTree::Punct(_) => "punctuation",
    }
}

struct TopIdent<'a> {
    ident: &'a Ident,
    exclaim: &'a Punct,
}
enum ParseTopIdentError {
    Msg(Spanned<Cow<'static, str>>),
    EndOfInput,
}
fn parse_top_ident(input: &[TokenTree]) -> Result<(&[TokenTree], TopIdent<'_>), ParseTopIdentError> {
    match input {
        [TokenTree::Ident(ident), TokenTree::Punct(punct), rest @ ..] if punct.as_char() == '!' => {
            Ok((rest, TopIdent {
                ident,
                exclaim: punct,
            }))
        },
        [TokenTree::Ident(ident), ..] => Err(ParseTopIdentError::Msg(
            Spanned::new(ident.span(),
                         Cow::Borrowed("top level identifiers must be suffixed with `!`")))),
        [tree, ..] => Err(ParseTopIdentError::Msg(Spanned::new(tree.span(),
                                                               Cow::Borrowed("expected identifier")))),
        [] => Err(ParseTopIdentError::EndOfInput)
    }
}

struct TermMatchArm {
    kind: TermKind,
    pattern_span: Span,
    if_matched: Vec<Spanned<TermCall>>,
}

fn parse_term_progn_body<'a>(input: &'a [TokenTree], errors: &mut ParseErrorCollection)
                             -> (&'a [TokenTree], Vec<Spanned<TermCall>>)
{
    let mut calls = Vec::new();
    let mut cursor = input;
    let offbase = |ptr: &[TokenTree]| {
        let ptr = ptr.as_ptr() as usize;
        ptr - (input.as_ptr() as usize)
    };
    while let [_, ..] = cursor {
        match parse_term(cursor) {
            Ok((rest, call)) => {
                // Assert that we're not staying in the same place.
                debug_assert_ne!(offbase(rest), offbase(cursor));
                calls.push(call);
                cursor = rest;
            },
            Err((rest, mut e)) => {
                // Assert that we're not staying in the same place.
                debug_assert_ne!(offbase(rest), offbase(cursor));
                errors.append(&mut e);
                cursor = rest;
            }
        }
    }
    (cursor, calls)
}

fn parse_term_match_arm(input: &[TokenTree])
                        -> Result<TermMatchArm, ParseErrorCollection>
{
    match input {
        [TokenTree::Punct(colon), TokenTree::Ident(pat_ident), rest @ ..] if colon.as_char() == ':' => {
            let kind = match &*pat_ident.to_string() {
                "dice" => Some(TermKind::Dice),
                "constant" => Some(TermKind::Constant),
                _ => todo!("handling malformed match patterns"),
            };
            let mut errors = ParseErrorCollection::new();
            let (rest, calls) = parse_term_progn_body(rest, &mut errors);
            if errors.errors.len() == 0 && matches!(kind, Some(_)) {
                Ok(TermMatchArm {
                    kind: kind.unwrap(),
                    pattern_span: pat_ident.span(),
                    if_matched: calls,
                })
            } else {
                Err(errors)
            }
        },
        [..] => todo!("handling malformed match arms"),
    }
}

fn parse_term_group(group: &Group) -> Result<Spanned<TermCall>, ParseErrorCollection> {
    let input = group.stream().into_iter().collect::<Vec<_>>();
    match &*input {
        [TokenTree::Ident(ident), rest @ ..] => {
            match &*ident.to_string() {
                "match" => match rest {
                    // Currently, it is only possible to match on kind,
                    // and there are only two kinds. That may change,
                    // but we're just making it work for now, lol.
                    // TODO: permit non exhaustive matching
                    [TokenTree::Ident(kind), TokenTree::Group(first_arm),
                     TokenTree::Group(second_arm)] if kind.to_string() == "kind" => {
                        let mut errors = ParseErrorCollection::new();
                        let first = first_arm.stream().into_iter().collect::<Vec<_>>();
                        let second = second_arm.stream().into_iter().collect::<Vec<_>>();
                        let first = parse_term_match_arm(&first);
                        let second = parse_term_match_arm(&second);
                        let (first, second) = match (first, second) {
                            (Ok(first), Ok(second)) => (first, second),
                            (Err(first), Ok(second)) => todo!("handling invalid match arms"),
                            (Ok(first), Err(second)) => todo!("handling invalid match arms"),
                            (Err(first), Err(second)) => todo!("handling invalid match arms"),
                        };
                        match (first.kind, second.kind) {
                            (TermKind::Dice, TermKind::Constant) => Ok(Spanned::new(
                                group.span(),
                                TermCall::Match(Box::new(TermMatch {
                                    dice_pattern: first.pattern_span,
                                    constant_pattern: second.pattern_span,
                                    dice: first.if_matched,
                                    constant: second.if_matched,
                                }))
                            )),
                            (TermKind::Constant, TermKind::Dice) => Ok(Spanned::new(
                                group.span(),
                                TermCall::Match(Box::new(TermMatch {
                                    dice_pattern: second.pattern_span,
                                    constant_pattern: first.pattern_span,
                                    dice: second.if_matched,
                                    constant: first.if_matched,
                                }))
                            )),
                            (TermKind::Dice, TermKind::Dice) => todo!("handling redundant patterns"),
                            (TermKind::Constant, TermKind::Constant) => todo!("handling redundant patterns"),
                        }
                    },
                    [..] => todo!("handle invalid match expressions"),
                },
                "sums" => match rest {
                    [TokenTree::Ident(same)] if same.to_string() == "same" =>
                        Ok(Spanned::new(group.span(),
                                        TermCall::InsertPartialSums(
                                            Spanned::new(same.span(),
                                                         PartialSumSignDirective::Same)))),
                    [TokenTree::Punct(plus)] if plus.as_char() == '+' =>
                        Ok(Spanned::new(group.span(),
                                        TermCall::InsertPartialSums(
                                            Spanned::new(plus.span(),
                                                         PartialSumSignDirective::Plus)))),
                    [_, _, ..] => todo!("handle extra arguments to partial sums directive"),
                    [_, ..] => todo!("handle invalid argument to partial sums directive"),
                    [] => todo!("handle missing argument to partial sums directive"),
                },
                _ => todo!("handle unknown term level call expressions"),
            }
        }
        _ => todo!("handling invalid term level call expressions")
    }
}

fn parse_term(input: &[TokenTree])
                   -> ParseResult<&[TokenTree], Spanned<TermCall>, ParseErrorCollection>
{
    match input {
        [TokenTree::Group(group), rest @ ..] => match parse_term_group(group) {
            Ok(call) => Ok((rest, call)),
            Err(e) => Err((rest, e)),
        },
        [TokenTree::Ident(ident), rest @ ..] => match &*ident.to_string() {
            "expr" => Ok((rest, Spanned::new(
                ident.span(),
                TermCall::InsertExpression
            ))),
            "total" => Ok((rest, Spanned::new(
                ident.span(),
                TermCall::InsertTotal
            ))),
            _ => todo!("handling unknown term level variables"),
        },
        [TokenTree::Literal(lit), rest @ ..] => if lit.is_string() {
            Ok((rest, Spanned::new(
                lit.span(),
                TermCall::InsertText(StringLiteral(lit.clone()))
            )))
        } else {
            todo!("handling term level non-string literals")
        },
        [TokenTree::Punct(punct), rest @ ..] => todo!("term level punctuation"),
        rest @ [] => Err((rest, ParseErrorCollection::new()))
    }
}

fn parse_join_body(input: &[TokenTree])
                   -> ParseResult<&[TokenTree],
                                  (Spanned<TermSeparator>, Vec<Spanned<TermCall>>),
                                  ParseErrorCollection>
{
    let mut errors = ParseErrorCollection::new();
    match input {
        [TokenTree::Ident(join_on), TokenTree::Punct(exclaim_join_on), TokenTree::Punct(colon_with),
         TokenTree::Ident(with), with_arg, TokenTree::Punct(colon_on),
         TokenTree::Ident(on), body @ ..] if
            join_on.to_string() == "terms" &&
            exclaim_join_on.as_char() == '!' &&
            colon_with.as_char() == ':' &&
            with.to_string() == "with" &&
            colon_on.as_char() == ':'
            => {
                let separator = match with_arg {
                    TokenTree::Ident(ident) => match &*ident.to_string() {
                        "operator" => Some(Spanned::new(ident.span(), TermSeparator::Operator)),
                        "comma" => Some(Spanned::new(ident.span(), TermSeparator::Comma)),
                        _ => todo!("special separators other than commas or operators"),
                    },
                    TokenTree::Literal(lit) => {
                        if lit.is_string() {
                            Some(Spanned::new(lit.span(),
                                              TermSeparator::Text(StringLiteral(lit.clone()))))
                        } else {
                            todo!("dealing with invalid literal separators")
                        }
                    }
                    _ => todo!("handling invalid separators"),
                };
                let (cursor, calls) = parse_term_progn_body(&body, &mut errors);
                if errors.errors.len() == 0 && matches!(separator, Some(_)) {
                    Ok((cursor, (separator.unwrap(), calls)))
                } else {
                    Err((cursor, errors))
                }
            },
        // TODO: combinators to make handling every case less of a chore
        [TokenTree::Ident(ident), ..] => todo!("better error reporting in join body parser"),
        [token, ..] => todo!("better error reporting in join body parser"),
        [] => todo!("better error reporting in join body parser"),
    }
}

// TODO: reconsider return type of this for the purpose of more error tolerant parsing
fn parse_top_group(group: &Group) -> Result<Spanned<TopCall>, ParseErrorCollection> {
    let input = group.stream().into_iter().collect::<Vec<_>>();
    let mut errors = ParseErrorCollection::new();
    match group.delimiter() {
        Delimiter::Parenthesis => (),
        Delimiter::Brace | Delimiter::Bracket | Delimiter::None => {
            errors.push(Spanned::new(group.span_open().join_first(group.span_close()),
                                     Cow::Borrowed("Invalid group delimiter kind. Use parentheses.")));
        },
    }
    match &*input {
        // TODO: actually parse top level groups
        [TokenTree::Ident(ident), rest @ ..] => {
            match &*ident.to_string() {
                "if" => match parse_top_ident(rest) {
                    Ok((rest, condition)) => {
                        let condition = match &*condition.ident.to_string() {
                            "many" => Some(Spanned::new(condition.ident.span()
                                                        .join_first(condition.exclaim.span()),
                                                        TopCondition::Many)),
                            _ => {
                                errors.push(
                                    Spanned::new(condition.ident.span().join_first(condition.exclaim.span()),
                                                 Cow::Borrowed(
                                                     "`many!` is the only valid top level if condition")));
                                None
                            },
                        };
                        // TODO: reject trailing input
                        match parse_top(rest) {
                            Ok((rest, Some(first))) => match parse_top(rest) {
                                Ok((rest, Some(second))) => {
                                    if let Some(condition) = condition {
                                        Ok(
                                            Spanned::new(
                                                group.span(),
                                                TopCall::If(condition, Box::new(first), Box::new(second))))
                                    } else {
                                        Err(errors)
                                    }
                                },
                                Ok((rest, None)) => todo!(),
                                Err((rest, e)) => todo!(),
                            },
                            // This actually needs to continue a loop.
                            Ok((rest, None)) => todo!(),
                            // We should continue parsing here, too.
                            Err((rest, e)) => todo!(),
                        }
                    },
                    // TODO: continue parsing to report more errors
                    Err(ParseTopIdentError::Msg(e)) => {
                        errors.push(e);
                        Err(errors)
                    },
                    Err(ParseTopIdentError::EndOfInput) => {
                        errors.push(
                        Spanned::new(group.span_close(),
                                     Cow::Borrowed("expected condition, found end of call expression")));
                        Err(errors)
                    },
                },
                "join" => match parse_join_body(rest) {
                    Ok((rest, (separator, body))) => Ok(Spanned::new(group.span(),
                                                                     TopCall::Join(separator, body))),
                    Err((rest, mut e)) => {
                        errors.append(&mut e);
                        Err(errors)
                    }
                },
                "progn" => match parse_progn_body(rest) {
                    (_, calls, errors) if errors.errors.len() == 0 => {
                        Ok(Spanned::new(group.span(),
                            TopCall::Progn(calls)))
                    },
                    _ => todo!(),
                },
                _ => todo!(),
            }
        },
        [tree, ..] => {
            errors.push(Spanned::new(tree.span(),
                                     Cow::Owned(format!("expected identifier, found {}", token_kind(tree)))));
            Err(errors)
        },
        [] => {
            errors.push(Spanned::new(group.span(),
                                     Cow::Borrowed("empty call expression")));
            Err(errors)
        },
    }
}

// This *may* continue parsing after encountering errors,
// but no parse tree will be returned, as the existing types
// are meant to be correct by construction.
fn parse_top(input: &[TokenTree]) -> ParseResult<&[TokenTree], Option<Spanned<TopCall>>, ParseErrorCollection> {
    let mut errors = ParseErrorCollection::new();
    match input {
        [TokenTree::Literal(literal), rest @ ..] => {
            if literal.is_string() {
                // TODO: change TopCall and such to borrow from the input,
                // so we can remove this clone.
                Ok((rest, Some(Spanned::new(literal.span(),
                                            TopCall::InsertText(StringLiteral(literal.clone()))))))
            } else {
                errors.push(Spanned::new(literal.span(), Cow::Borrowed("unexpected literal")));
                Err((rest, errors))
            }
        },
        [TokenTree::Ident(ident), TokenTree::Punct(punct), rest @ ..] => {
            if punct.as_char() == '!' {
                match &*ident.to_string() {
                    "total" => Ok((rest, Some(Spanned::new(ident.span().join_first(punct.span()),
                                                           TopCall::InsertTotal)))),
                    "many" => {
                        errors.push(Spanned::new(ident.span().join_first(punct.span()),
                                                 Cow::Borrowed("use of `many!` outside of condition")));
                        Err((rest, errors))
                    },
                    _ => {
                        errors.push(Spanned::new(ident.span().join_first(punct.span()),
                                    Cow::Owned(format!("unknown variable `{}!`", ident))));
                        Err((rest, errors))
                    }
                }
            } else {
                errors.push(Spanned::new(ident.span().join_first(punct.span()),
                                         Cow::Borrowed("top level identifiers must be suffixed with `!`")));
                Err((rest, errors))
            }
        },
        [TokenTree::Ident(ident), rest @ ..] => {
            errors.push(Spanned::new(ident.span(),
                                     Cow::Borrowed("top level identifiers must be suffixed with `!`")));
            Err((rest, errors))
        }
        [TokenTree::Group(group), rest @ ..] => {
            match parse_top_group(group) {
                Ok(call) => Ok((rest, Some(call))),
                Err(e) => Err((rest, e)),
            }
        },
        [TokenTree::Punct(punct), rest @ ..] => {
            if punct.as_char() == ';' {
                Ok((rest, None))
            } else {
                errors.push(Spanned::new(punct.span(),
                                         Cow::Owned(format!("unexpected punctuation `{}`", punct))));
                Err((rest, errors))
            }
        },
        // Ignore End of Input.
        rest @ [] => Ok((rest, None)),
    }
}

/// Parse the body of a `progn`.
fn parse_progn_body(mut input: &[TokenTree]) -> (&[TokenTree], Vec<Spanned<TopCall>>, ParseErrorCollection) {
    let mut calls = Vec::new();
    let mut parse_errors = ParseErrorCollection::new();
    while let [_, ..] = input {
        match parse_top(input) {
            Ok((rest, Some(call))) => {
                calls.push(call);
                input = rest;
            },
            Ok((rest, None)) => input = rest,
            Err((rest, mut e)) => {
                parse_errors.append(&mut e);
                input = rest;
            }
        }
    }
    (input, calls, parse_errors)
}

/// High level representation of a whole NFMT program.
pub(crate) struct Program {
    meta: Meta,
    implicit_progn: Vec<Spanned<TopCall>>,
    parse_errors: ParseErrorCollection,
}

impl Program {
    /// Parse a program from input.
    pub(crate) fn parse(input: TokenStream) -> Program {
        let input = input.into_iter().collect::<Vec<_>>();
        let input = &*input;
        // TODO: parse metadata block
        let mut meta = Meta::new();
        let (rest, implicit_progn, parse_errors) = parse_progn_body(input);
        dbg!(&implicit_progn);
        Self { meta, implicit_progn, parse_errors }
    }
    pub(crate) fn into_code(self) -> TokenStream {
        let errors = self.parse_errors.into_iter().map(|Spanned { span, val }| {
            ::quote::quote_spanned!(span=> compile_error!(#val))
        });
        let lowered = lower(Spanned::new(Span::call_site(), &*self.implicit_progn));
        let code = codegen(&lowered.val);
        ::quote::quote! {
            { #( #errors );* #code }
        }
    }
}

/// A singly linked list structure meant for
/// passing wrapped contexts down during lowering.
struct StackList<'a, T> {
    head: T,
    tail: Option<&'a StackList<'a, T>>,
}
fn cons<'a, 'b, T>(head: T, tail: &'a StackList<'b, T>) -> StackList<'a, T>
where 'b: 'a
{
    StackList {
        tail: Some(tail),
        head,
    }
}
fn with_cons<T, R, F: FnOnce(&StackList<'_, T>) -> R>(head: T, tail: &StackList<'_, T>, func: F) -> R {
    let list = cons(head, &tail);
    func(&list)
}
impl<'a, T> StackList<'a, T> {
    fn new(head: T) -> Self {
        Self {
            tail: None,
            head,
        }
    }
    fn cons<'b>(&'a self, head: T) -> StackList<'b, T>
        where 'a: 'b,
    {
        StackList {
            tail: Some(self),
            head,
        }
    }
    fn with_cons<R, F: FnOnce(&StackList<'_, T>) -> R>(&self, head: T, func: F) -> R {
        let list = cons(head, self);
        func(&list)
    }
    fn iter(&self) -> impl Iterator<Item = &T> {
        let mut tail = self.tail;
        iter::once(&self.head).chain(
            iter::from_fn(move || {
                match tail {
                    Some(ptr) => {
                        tail = ptr.tail;
                        Some(&ptr.head)
                    },
                    None => None,
                }
            })
        )
    }
}

enum IfArm {
    True,
    False,
}

enum TopContext {
    If(Spanned<TopCondition>, IfArm),
    Normal,
}
/// Wrap a format item with context, like being conditional on something.
fn context_wrap(context: &Spanned<TopContext>, item: Spanned<FormatItem>) -> Spanned<FormatItem> {
    match &context.val {
        TopContext::If(condition, IfArm::True) => {
            Spanned::new(condition.span, FormatItem::IfMany(Box::new(item)))
        },
        TopContext::If(condition, IfArm::False) => {
            Spanned::new(condition.span, FormatItem::IfNotMany(Box::new(item)))
        },
        TopContext::Normal => item,
    }
}
fn wrap_with(contexts: &StackList<'_, Spanned<TopContext>>, item: Spanned<FormatItem>)
             -> Spanned<FormatItem>
{
    contexts.iter().fold(item, |a, context| context_wrap(context, a))
}

// TODO: examine what to do when we encounter limits in codegen
fn lower_top(context: &StackList<'_, Spanned<TopContext>>, call: &Spanned<TopCall>, out: &mut Vec<Spanned<FormatItem>>)  {
    match &call.val {
        TopCall::InsertTotal => {
            out.push(wrap_with(context, Spanned::new(call.span, FormatItem::InsertTotal)))
        },
        TopCall::Join(separator, terms) => {
            // TODO: fix up spans here
            let term_context_list = StackList::new(Spanned::new(call.span, TermContext::Normal));
            let mut lowered = Vec::new();
            for term in terms {
                lower_term(&term_context_list, term, &mut lowered);
            }
            out.push(wrap_with(context,
                               Spanned::new(call.span, FormatItem::InsertTerms(separator.clone(), lowered))));
        },
        TopCall::InsertText(text) => {
            // TODO: consider consuming the high level representation instead of borrowing,
            // so we can avoid this clone
            out.push(wrap_with(context, Spanned::new(call.span, FormatItem::InsertText(text.clone()))))
        },
        // TODO: examine whether this assigns spans correctly
        // TODO: examine whether we can infer when the conditional
        // typing limitation doesn't matter, so we can unify elements of the two arms
        // And, examine whether that will have any meaningful impact on runtime performance
        TopCall::If(condition, truth, fiction) => {
            context.with_cons(Spanned::new(call.span, TopContext::If(*condition, IfArm::True)), |context| {
                lower_top(context, truth, out);
            });
            context.with_cons(Spanned::new(call.span, TopContext::If(*condition, IfArm::False)), |context| {
                lower_top(context, fiction, out);
            });
        },
        // TODO: consider giving progns a context
        TopCall::Progn(calls) => {
            calls.iter().for_each(|inner_call| {
                lower_top(context, inner_call, out);
            });
        },
    }
}

enum TermContext {
    IfKind(Spanned<TermKind>),
    Normal,
}

fn term_context_wrap(context: &Spanned<TermContext>, item: Spanned<TermFormatItem>) -> Spanned<TermFormatItem> {
    match &context.val {
        TermContext::IfKind(kind) => {
            Spanned::new(context.span, TermFormatItem::IfKind(*kind, Box::new(item)))
        },
        TermContext::Normal => item,
    }
}
fn wrap_term_with(contexts: &StackList<'_, Spanned<TermContext>>, item: Spanned<TermFormatItem>)
                  -> Spanned<TermFormatItem>
{
    contexts.iter().fold(item, |a, context| term_context_wrap(context, a))
}

fn lower_term(context: &StackList<'_, Spanned<TermContext>>, call: &Spanned<TermCall>, out: &mut Vec<Spanned<TermFormatItem>>) {
    match &call.val {
        TermCall::InsertTotal => {
            out.push(wrap_term_with(context, Spanned::new(call.span, TermFormatItem::InsertTotal)))
        },
        TermCall::InsertPartialSums(directive) => {
            out.push(wrap_term_with(context,
                                    Spanned::new(call.span, TermFormatItem::InsertPartialSums(*directive))))
        },
        TermCall::InsertExpression => {
            out.push(wrap_term_with(context, Spanned::new(call.span, TermFormatItem::InsertExpression)))
        },
        TermCall::InsertText(text) => {
            out.push(wrap_term_with(context,
                                    Spanned::new(call.span, TermFormatItem::InsertText(text.clone()))))
        },
        TermCall::Match(m) => {
            // TODO: support non exhaustive match
            let TermMatch { dice_pattern, constant_pattern, dice, constant } = &**m;
            context.with_cons(
                Spanned::new(call.span,
                             TermContext::IfKind(Spanned::new(*dice_pattern, TermKind::Dice))),
                |context| {
                    dice.iter().for_each(|inner_call| {
                        lower_term(context, inner_call, out);
                    });
                });
            context.with_cons(
                Spanned::new(
                    call.span, TermContext::IfKind(Spanned::new(*constant_pattern, TermKind::Constant))),
                |context| {
                    constant.iter().for_each(|inner_call| {
                        lower_term(context, inner_call, out);
                    });
                });
        }
        _ => todo!()
    }
}

fn lower(program: Spanned<&[Spanned<TopCall>]>) -> Spanned<Vec<Spanned<FormatItem>>> {
    let top_context = Spanned::new(program.span, TopContext::Normal);
    let context_list = StackList::new(top_context);
    let mut lowered = Vec::new();
    for call in program.val {
        lower_top(&context_list, &call, &mut lowered);
    }
    Spanned::new(program.span, lowered)
}

fn cons_list_ty(list: &[TokenStream]) -> TokenStream {
    // TODO: implement this without the macro_rules! macro
    ::quote::quote! {
        ::mice::list_ty![
            #( #list ),*
        ]
    }
}

fn cons_list(list: &[TokenStream]) -> TokenStream {
    // TODO: implement this without the macro_rules! macro
    ::quote::quote! {
        ::mice::list![
            #( #list ),*
        ]
    }
}

struct NeededPaths {
    /// Path to the `nfmt` module of the `mice` crate.
    nfmt: TokenStream,
    /// Path to `borrow::Cow` in the standard library.
    cow: TokenStream,
}

impl Default for NeededPaths {
    fn default() -> Self {
        Self {
            nfmt: ::quote::quote!(crate),
            cow: ::quote::quote!(::std::borrow::Cow),
        }
    }
}
trait ToTokens {
    fn to_tokens(&self, paths: &NeededPaths, tokens: &mut TokenStream);
    fn to_token_stream(&self, paths: &NeededPaths) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens(paths, &mut tokens);
        tokens
    }
}


// Note that if ToTokens turns out to be too limited,
// defining our own trait for this purpose is perfectly well
// in the cards.
impl<T> ::quote::ToTokens for Spanned<T>
where T: ::quote::ToTokens {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Spanned { span, ref val } = *self;
        tokens.extend(::quote::quote_spanned! {
            span=> #val
        })
    }
}

impl ::quote::ToTokens for TermFormatItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        eprintln!("TODO: codegen terms");
    }
}

impl ::quote::ToTokens for FormatItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use ::quote::{quote, quote_spanned};
        match self {
            FormatItem::InsertTotal => tokens.extend(quote! {
                crate::nfmt::FormatItem::Total
            }),
            FormatItem::InsertTerms(separator, items) => {
                let separator = match &separator.val {
                    TermSeparator::Operator => quote! { crate::nfmt::TermSeparator::Operator },
                    TermSeparator::Comma => quote! { crate::nfmt::TermSeparator::Comma },
                    TermSeparator::Text(StringLiteral(text)) => quote! {
                        crate::nfmt::TermSeparator::Text(
                            ::std::borrow::Cow::Borrowed(#text)
                        )
                    },
                };
                tokens.extend(quote! {
                    crate::nfmt::FormatItem::Terms(#separator, crate::nfmt::TermFormatDescriptor {
                        // TODO: implement list! internally
                        items: crate::list![ #( #items ),* ]
                    })
                });
            },
            FormatItem::InsertText(StringLiteral(text)) => {
                tokens.extend(quote! {
                    // TODO: pass path to Cow as well
                   crate::nfmt::FormatItem::Text(::std::borrow::Cow::Borrowed(#text))
                });
            },
            FormatItem::IfMany(item) => {
                tokens.extend(quote! {
                    crate::nfmt::FormatItem::GlobalPropertyDependent(
                        crate::nfmt::IfMany {
                            item: #item
                        }
                    )
                });
            },
            FormatItem::IfNotMany(item) => {
                tokens.extend(quote! {
                    crate::nfmt::FormatItem::GlobalPropertyDependent(
                        crate::nfmt::IfNotMany {
                            item: #item
                        }
                    )
                });
            },
            FormatItem::Nothing => tokens.extend(quote! {
                crate::nfmt::FormatItem::Nothing
            }),
            _ => todo!(),
        }
    }
}

// TODO: receive `mice` crate path as argument,
// so we can have a wrapper macro_rules! macro that uses
// the $crate syntax variable to get it independent of renaming.
fn codegen(program: &[Spanned<FormatItem>]) -> TokenStream {
    use ::quote::{quote, quote_spanned, ToTokens};
    let mut output = TokenStream::new();
    output.extend(quote! {
        (
            #( #program ),*
        )
    });
    output
}
