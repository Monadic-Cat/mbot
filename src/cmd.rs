use std::io;
use std::io::{BufRead, Write};
use mice::unstable::parse::{integer, is_dec_digit};
use serenity::{
    model::{channel::Message, gateway::Ready, id::ChannelId},
    prelude::*,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::one_of,
    combinator::{opt, peek},
    multi::{many0, many1},
    sequence::tuple,
    IResult,
};

fn whitespace(input: &str) -> IResult<&str, &str> {
    alt((tag(" "), tag("\t"), tag("\n")))(input)
}

enum Command<'a> {
    Say(ChannelId, &'a str),
    SayImplicitChannel(&'a str),
    SelectChannel(ChannelId),
    ListGuilds,
    Shutdown,
}

enum ParseError {
    InvalidCommand,
}

fn parse_command<'a>(input: &'a str) -> Result<Command<'a>, ParseError> {
    use nom::Err::Failure;
    use nom::error::ErrorKind::TooLarge;
    alt((
        |i| {
            let (i, _) = alt((tag("!"), tag("say")))(i)?;
            let (i, _) = many1(whitespace)(i)?;
            Ok((i, Command::SayImplicitChannel(i)))
        },
        |i| {
            let (i, _) = tag("select")(i)?;
            let (i, _) = many1(whitespace)(i)?;
            let (i, s) = take_while1(is_dec_digit)(i)?;
            let n = match s.parse::<u64>() {
                Ok(x) => ChannelId(x),
                Err(_) => Err(Failure((i, TooLarge)))?,
            };
            Ok((i, Command::SelectChannel(n)))
        },
        |i| {
            let (i, _) = tag("ls")(i)?;
            Ok((i, Command::ListGuilds))
        },
        |i| tag("shutdown")(i).map(|(i, _)| (i, Command::Shutdown)),
    ))(input).map_err(|_| ParseError::InvalidCommand).map(|(_, x)| x)
}

pub(crate) enum Error {
    IO,
    CmdParse,
}
impl From<io::Error> for Error {
    fn from(_: io::Error) -> Self {
        Self::IO
    }
}
impl From<ParseError> for Error {
    fn from(_: ParseError) -> Self {
        Self::CmdParse
    }
}

/// When the `!` type is stabilized, replace this with it.
pub(crate) enum Never {}

/// A terminal command loop.
/// This reads lines from STDIN and acts on them with
/// the given Serenity context.
/// It's an error if this returns.
pub(crate) async fn command_loop(ctx: Context, ready: Ready) -> Result<Never, Error> {
    macro_rules! prompt {
        ($out:ident) => {
            let _ = write!($out, "mbot~$ ");
            let _ = $out.flush();
        }
    }
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut current_channel: Option<ChannelId> = None;

    prompt!(stdout);

    let readline = || -> Result<String, io::Error> {
        let mut input = String::new();
        stdin.read_line(&mut input)?;
        Ok(input)
    };
    while let Ok(line) = readline() {
        let cmd = match parse_command(&line) {
            Ok(x) => x,
            Err(_) => {
                println!("[ERROR]: Invalid Command");
                prompt!(stdout);
                continue
            },
        };
        match cmd {
            Command::Say(id, text) => {
                match id.say(&ctx.http, text).await {
                    Ok(_) => (),
                    Err(x) => println!("[ERROR]: {}", x),
                };
            },
            Command::SayImplicitChannel(text) => {
                if let Some(current_channel) = current_channel {
                    match current_channel.say(&ctx.http, text).await {
                        Ok(_) => (),
                        Err(x) => println!("[ERROR]: {}", x),
                    };
                } else {
                    println!("[ERROR]: No channel selected!");
                }
            },
            Command::SelectChannel(id) => {
                current_channel = Some(id);
            },
            Command::ListGuilds => {
                let guilds = ready.user.guilds(&ctx.http).await;
                match guilds {
                    Ok(x) => {
                        let list = x.into_iter().map(|x| x.name).collect::<Vec<_>>();
                        println!("{:?}", list);
                    },
                    Err(x) => println!("[ERROR]: {:#?}", x),
                }
            },
            Command::Shutdown => {
                std::process::exit(0)
            }
        };

        prompt!(stdout);
    }

    Err(Error::IO) // stdin closed
}
