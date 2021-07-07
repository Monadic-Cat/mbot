//! Parsing roll commands from normal messages,
//! 
//! in the form of "...normal text...(roll: &lt;dice expression>)...normal text..."
use ::core::convert::TryInto;
use pulldown_cmark as cmark;
use ::mice::parse::{Program, ExprError, parse_expression};

enum InlineRollError {
    TooLarge,
    InvalidDie,
    /// Any error or condition which we do not report.
    Silent,
}

#[derive(Debug)]
enum ReportedError {
    TooLarge,
    InvalidDie,
}
impl ::core::convert::TryFrom<InlineRollError> for ReportedError {
    type Error = ();
    fn try_from(e: InlineRollError) -> Result<Self, Self::Error> {
        match e {
            InlineRollError::TooLarge => Ok(Self::TooLarge),
            InlineRollError::InvalidDie => Ok(Self::InvalidDie),
            _ => Err(()),
        }
    }
}

fn inline_roll(input: &[u8]) -> Result<(&[u8], Program), (&[u8], InlineRollError)> {
    match input {
        [b'(', b'r', b'o', b'l', b'l', b':', rest @ ..] | [b'(', b'r', b':', rest @ ..] => {
            match parse_expression(rest) {
                Ok(([b')', rest @ ..], (_tokens, proggy))) => Ok((rest, proggy)),
                Ok((rest, _)) => Err((rest, InlineRollError::Silent)),
                Err(([b')', rest @ ..], ExprError::TooLarge)) => Err((rest, InlineRollError::TooLarge)),
                Err(([b')', rest @ ..], ExprError::InvalidDie)) => Err((rest, InlineRollError::InvalidDie)),
                Err((rest, _)) => Err((rest, InlineRollError::Silent)),
            }
        },
        _ => Err((input, InlineRollError::Silent)),
    }
}

#[derive(Debug)]
pub(crate) struct ParsedMessage {
    rolls: Vec<Result<Program, ReportedError>>,
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
            match inline_roll(place.as_bytes()).map_err(|(r, e)| (r, e.try_into().ok())) {
                // The current place is a valid internal roll command.
                // Store the parsed result of that, and scroll past.
                Ok((input, res)) => {
                    info.rolls.push(Ok(res));
                    place = &place[(input.as_ptr() as usize) - (place.as_ptr() as usize) ..];
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
                Err((_rest, Some(e))) => {
                    info.rolls.push(Err(e));
                    place = match shrunk_slice(place) {
                        Some(x) => x,
                        None => break,
                    };
                },
                Err((_rest, None)) => place = match shrunk_slice(place) {
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

pub fn response_for(input: &str) -> Option<String> {
    use ::mice::{interp::{interpret, InterpError, fmt::mbot_format_default}, cost::{cost, Price, AstInterp, mbot::{self, TextFormatOutput}}};
    let info = message(input);
    if !info.rolls.is_empty() {
        let cost: Price = info.rolls.iter()
            .filter_map(|x| x.as_ref().ok())
            .map(|x| cost::<AstInterp, _>(x, ()) + cost::<TextFormatOutput<mbot::Default>, _>(x, ()))
            .sum();
        match cost {
            Price::Bounded(cost) if cost <= 10000 => (),
            _ => return None,
        }
        let response = info.rolls.iter().map(|report| {
            match report {
                Ok(proggy) => Ok(interpret(&mut ::rand::thread_rng(), proggy).map(|output| (proggy, output))),
                Err(e) => Err(e),
            }
        }).map(|result| match result {
            Ok(Ok((proggy, output))) => mbot_format_default(proggy.terms(), &output),
            Ok(Err(InterpError::OverflowPositive)) => String::from("sum is too high for `i64`"),
            Ok(Err(InterpError::OverflowNegative)) => String::from("sum is too low for `i64`"),
            Err(ReportedError::TooLarge) => String::from("Invalid die"),
            Err(ReportedError::InvalidDie) => String::from("Invalid die"),
        }).collect::<Vec<_>>().join("\n");
        Some(response)
    } else {
        None
    }
}
