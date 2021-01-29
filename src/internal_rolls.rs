//! Parsing roll commands from normal messages,
//! 
//! in the form of "...normal text...(roll: &lt;dice expression>)...normal text..."
use ::mice::parse::{dice, whitespace, Expression, InvalidDie};
use ::mice::{ExpressionResult, FormatOptions};
use ::nom::{bytes::complete::tag, multi::many0, sequence::tuple, branch::alt, IResult};
use pulldown_cmark as cmark;

fn internal_roll(input: &str) -> IResult<&str, Result<Expression, InvalidDie>> {
    let (input, (_, _, res, _, _)) = tuple((
        alt((tag("(roll:"), tag("(r:"))),
        many0(whitespace),
        dice,
        many0(whitespace),
        tag(")"),
    ))(input)?;
    Ok((input, res))
}

#[derive(Debug)]
pub(crate) struct ParsedMessage {
    rolls: Vec<Result<Expression, InvalidDie>>,
}
impl ParsedMessage {
    fn new() -> Self {
        Self {
            rolls: Vec::new(),
        }
    }
}

/// Create right shifted valid UTF-8 slice reference.
fn shrunk_slice(input: &str) -> Option<&str> {
    let mut iter = input.char_indices();
    iter.next();
    iter.next().map(|(idx, _)| &input[idx..])
}

#[derive(Clone, Copy)]
enum ParseState {
    Normal,
    InCodeBlock,
}

/// *All* messages are valid.
///
/// *Some* messages contain rolls for us to handle.
///
/// *Some* messages contain syntactically valid rolls that have no valid evaluation tactic.
pub(crate) fn message(input: &str) -> ParsedMessage {
    let mut info = ParsedMessage::new();
    let mut paragraph = |mut place: &str| {
        // Time to scroll across the thing uwu
        while place.len() > 1 {
            match internal_roll(place) {
                // The current place is a valid internal roll command.
                // Store the parsed result of that, and scroll past.
                Ok((input, res)) => {
                    info.rolls.push(res);
                    place = input;
                }
                // The current place is not a syntactically valid internal roll command.
                // That does not make this message invalid.
                // Just move forward one step.

                // For this particular case, it would make sense to scroll all the way to the end,
                // but mice::dice backs out early from dice expressions like this.
                // Considering all the work it does to construct dice when they are valid,
                // it should actually be faster to scroll one by one across from this loop,
                // because the tag we use will never show up in a dice expression, so
                // the parser will back out faster.
                // (As opposed to making mice::dice more tolerant of these errors.)
                // If we ever want to report the error in more detail, though,
                // we'll want to consume the whole thing and keep spans.
                Err(::nom::Err::Failure((_, ::nom::error::ErrorKind::TooLarge))) => {
                    info.rolls.push(Err(InvalidDie));
                    place = match shrunk_slice(place) {
                        Some(x) => x,
                        None => break,
                    };
                },
                _ => place = match shrunk_slice(place) {
                    Some(x) => x,
                    None => break,
                },
            }
        }
    };
    let parser = cmark::Parser::new(input);
    let mut state = ParseState::Normal;
    for event in parser {
        use ParseState::{Normal, InCodeBlock};
        use cmark::{Event, Tag};
        match (state, event) {
            (Normal, Event::Text(text)) => paragraph(&text),
            (Normal, Event::Code(_)) => (),
            (Normal, Event::Start(Tag::CodeBlock(_))) => state = InCodeBlock,
            (InCodeBlock, Event::End(Tag::CodeBlock(_))) => state = Normal,
            _ => (),
        }
    }
    info
}

pub(crate) fn response_for(input: &str) -> Option<String> {
    let info = message(input);
    if info.rolls.len() > 0 {
        use ::mice::util::ExpressionExt;
        let cost: i64 = info.rolls.iter()
            .filter_map(|x| x.as_ref().ok())
            .map(|x| x.evaluation_cost(Some(10000)))
            .sum();
        if cost > 10000 {
            return None;
        }
        let results: Vec<Result<ExpressionResult, _>> =
            info.rolls.into_iter().map(|x| match x.map(|x| x.roll()) {
                Ok(Ok(x)) => Ok(x),
                Ok(Err(e)) => Err(e),
                Err(InvalidDie) => Err(::mice::Error::InvalidDie),
            }).collect();
        Some(results.into_iter().map(|x| match x {
            Ok(x) => x.format(FormatOptions::new().total_right()),
            Err(e) => format!("{}", e),
        }).collect::<Vec<_>>().join("\n"))
    } else {
        None
    }
}
