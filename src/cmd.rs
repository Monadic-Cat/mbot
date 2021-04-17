use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    multi::many1,
    IResult,
};
use serenity::{
    model::{gateway::Ready, id::ChannelId},
    prelude::*,
};
use std::io;
use std::io::Write;
use std::sync::Arc;

fn whitespace(input: &str) -> IResult<&str, &str> {
    alt((tag(" "), tag("\t"), tag("\n")))(input)
}

enum Command<'a> {
    // Say(ChannelId, &'a str),
    SayImplicitChannel(&'a str),
    SelectChannel(ChannelId),
    ListGuilds,
    #[cfg(feature = "plotting")]
    ReloadPlotter,
    Shutdown,
}

enum ParseError {
    InvalidCommand,
}

fn parse_command(input: &str) -> Result<Command<'_>, ParseError> {
    use nom::error::ErrorKind::TooLarge;
    use nom::Err::Failure;
    alt((
        |i| {
            let (i, _) = alt((tag("!"), tag("say")))(i)?;
            let (i, _) = many1(whitespace)(i)?;
            Ok((i, Command::SayImplicitChannel(i)))
        },
        |i| {
            let (i, _) = tag("select")(i)?;
            let (i, _) = many1(whitespace)(i)?;
            let (i, s) = take_while1(|c: char| c.is_digit(10))(i)?;
            let n = match s.parse::<u64>() {
                Ok(x) => ChannelId(x),
                Err(_) => return Err(Failure((i, TooLarge))),
            };
            Ok((i, Command::SelectChannel(n)))
        },
        |i| {
            let (i, _) = tag("ls")(i)?;
            Ok((i, Command::ListGuilds))
        },
        |i| tag("shutdown")(i).map(|(i, _)| (i, Command::Shutdown)),
        #[cfg(feature = "plotting")]
        |i| tag("reload plotter")(i).map(|(i, _)| (i, Command::ReloadPlotter)),
    ))(input)
    .map_err(|_| ParseError::InvalidCommand)
    .map(|(_, x)| x)
}
use ::thiserror::Error;
#[derive(Error, Debug)]
pub(crate) enum Error {
    #[error("cmdline io error")]
    IO,
    #[error("error parsing command")]
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

#[derive(Error, Debug)]
enum ReadlineError {
    #[error("{0}")]
    Join(#[from] ::tokio::task::JoinError),
    #[error("{0}")]
    IO(#[from] io::Error),
}

async fn areadline(stdin: Arc<io::Stdin>) -> Result<String, ReadlineError> {
    let output: Result<Result<_, io::Error>, _> = ::tokio::task::spawn_blocking(move || {
        let mut input = String::new();
        stdin.read_line(&mut input)?;
        Ok(input)
    })
    .await;
    match output {
        Ok(Ok(x)) => Ok(x),
        Ok(Err(e)) => Err(e.into()),
        Err(e) => Err(e.into()),
    }
}

/// A terminal command loop.
/// This reads lines from STDIN and acts on them with
/// the given Serenity context.
/// It's an error if this returns.
pub(crate) async fn command_loop(ctx: Context, ready: Ready) -> Result<Never, Error> {
    macro_rules! prompt {
        ($out:ident) => {
            let _ = write!($out, "mbot~$ ");
            let _ = $out.flush();
        };
    }
    let stdin = Arc::new(io::stdin());
    let mut stdout = io::stdout();
    let mut current_channel: Option<ChannelId> = None;

    prompt!(stdout);

    while let Ok(line) = areadline(stdin.clone()).await {
        let cmd = match parse_command(&line) {
            Ok(x) => x,
            Err(_) => {
                println!("[ERROR]: Invalid Command");
                prompt!(stdout);
                continue;
            }
        };
        match cmd {
            // Command::Say(id, text) => {
            //     match id.say(&ctx.http, text).await {
            //         Ok(_) => (),
            //         Err(x) => println!("[ERROR]: {}", x),
            //     };
            // }
            Command::SayImplicitChannel(text) => {
                if let Some(current_channel) = current_channel {
                    match current_channel.say(&ctx.http, text).await {
                        Ok(_) => (),
                        Err(x) => println!("[ERROR]: {}", x),
                    };
                } else {
                    println!("[ERROR]: No channel selected!");
                }
            }
            Command::SelectChannel(id) => {
                current_channel = Some(id);
            }
            Command::ListGuilds => {
                let guilds = ready.user.guilds(&ctx.http).await;
                match guilds {
                    Ok(x) => {
                        let list = x.into_iter().map(|x| x.name).collect::<Vec<_>>();
                        println!("{:?}", list);
                    }
                    Err(x) => println!("[ERROR]: {:#?}", x),
                }
            }
            #[cfg(feature = "plotting")]
            Command::ReloadPlotter => {
                ::tokio::task::spawn_blocking(|| crate::dist::Plotter::reload()).await.unwrap();
            },
            Command::Shutdown => crate::shutdown().await,
        };

        prompt!(stdout);
    }

    Err(Error::IO) // stdin closed
}
