//! Parsing roll commands from normal messages,
//! 
//! in the form of "...normal text...(roll: &lt;dice expression>)...normal text..."
use ::mice::parse::{dice, whitespace, Expression, InvalidDie};
use ::nom::{bytes::complete::tag, multi::many0, sequence::tuple, IResult};

fn internal_roll(input: &str) -> IResult<&str, Result<Expression, InvalidDie>> {
    let (input, (_, _, res, _, _)) = tuple((
        tag("(roll:"),
        many0(whitespace),
        dice,
        many0(whitespace),
        tag(")"),
    ))(input)?;
    Ok((input, res))
}

#[derive(Debug)]
struct FoundRolls {
    rolls: Vec<Result<Expression, InvalidDie>>,
}
impl FoundRolls {
    fn new() -> Self {
        Self { rolls: Vec::new() }
    }
    fn push(&mut self, val: Result<Expression, InvalidDie>) {
        self.rolls.push(val)
    }
}
#[derive(Debug)]
pub(crate) struct ParsedMessage {
    rolls: FoundRolls,
}
impl ParsedMessage {
    fn new() -> Self {
        Self {
            rolls: FoundRolls::new(),
        }
    }
}

/// *All* messages are valid.
///
/// *Some* messages contain rolls for us to handle.
///
/// *Some* messages contain syntactically valid rolls that have no valid evaluation tactic.
pub(crate) fn message(input: &str) -> ParsedMessage {
    // Time to scroll across the thing uwu
    let mut place = input;
    let mut info = ParsedMessage::new();
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
            // Note that this can panic by creating an invalid UTF-8 slice.
            // We should probably change the dice parsers,
            // which do not inherently require valid UTF-8 input.
            // That change would essentially be the replacement of
            // `&str` with `&[u8]` in a number of places.
            // It may also be useful to make an iterator
            // for the creation of shrinking slices across our input.
            Err(::nom::Err::Failure((_, ::nom::error::ErrorKind::TooLarge))) => {
                place = &place[1..];
                info.rolls.push(Err(InvalidDie));
            },
            _ => place = &place[1..],
        }
    }
    info
}
