#![cfg_attr(not(feature = "reloadable_plotter"), forbid(unsafe_code))]
mod initiative;
use initiative::pathfinder_initiative;
#[cfg(feature = "internal_rolls")]
mod internal_rolls;
#[cfg(feature = "cli_control")]
mod cmd;
#[cfg(feature = "cli_control")]
use cmd::command_loop;
#[cfg(feature = "control_socket")]
mod control_socket;
#[cfg(feature = "maddie_tools")]
mod masks;
#[cfg(feature = "plotting")]
mod dist;

use serenity::{
    framework::standard::{
        macros::{command, group},
        Args,
        CommandResult,
        StandardFramework,
    },
    model::{
        channel::Message,
        gateway::Ready,
        id::ChannelId,
    },
    client::bridge::gateway::ShardManager,
    prelude::*,
};
use std::sync::Arc;
use tokio::task;

async fn reply(ctx: &Context, msg: &Message, r: &str) -> CommandResult {
    let instance_name = std::env::var("INSTANCE_MESSAGE_PREFIX").unwrap_or_default();
    msg.channel_id
        .say(
            &ctx.http,
            format!("{}{} {}", instance_name, msg.author.mention(), r),
        )
        .await?;
    Ok(())
}

#[command]
async fn ping(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "pong").await
}
// Suggested by Lasersquid
#[command]
async fn potatoes(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "tatoes").await
}
#[command]
async fn to(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "matoes").await
}
#[command]
async fn happy(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "La la la la la la! Sing the happy song!").await
}
#[command]
async fn po(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "tatoes").await
}
#[command]
async fn literal(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    reply(ctx, msg, &format!("Literal {}", arg.message().trim())).await
}
#[command]
async fn gargamel(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "I think you mean \"Gargalsmell\"!").await
}

const MAX_REPLY_LENGTH: usize = 1900;

fn ast_format_smart(input: &::mice::parse::Program, output: &::mice::interp::ProgramOutput) -> String {
    use ::mice::interp;
    let first = interp::fmt::mbot_format_default(input.terms(), output);
    let second;
    #[allow(clippy::blocks_in_if_conditions)]
    if first.len() < MAX_REPLY_LENGTH {
        first
    } else if {
        second = interp::fmt::mbot_format_short(input.terms(), output);
        second.len() < MAX_REPLY_LENGTH
    } {
        second
    } else {
        output.total().to_string()
    }
}

#[derive(Debug)]
enum MaybeReasonedDice<'a> {
    Reasoned(::mice::parse::Program, &'a str),
    Unreasoned(::mice::parse::Program),
}

enum ReasonedDiceError {
    Expr(::mice::parse::ExprError),
    UnknownTrailing,
}

fn reasoned_dice(input: &str) -> Result<MaybeReasonedDice<'_>, ReasonedDiceError> {
    use ::mice::parse::{parse_expression, Token};
    let (inp, (tokens, program)) = match parse_expression(input.as_bytes()) {
        Ok((input, x)) => (input, x),
        Err((_, e)) => return Err(ReasonedDiceError::Expr(e)),
    };

    if !inp.is_empty() {
        match tokens.last() {
            Some(Token::Whitespace) => match inp {
                [b't', b'o', ..] | [b'f', b'o', b'r', ..] |
                [b'b', b'e', b'c', b'a', b'u', b's', b'e', ..] |
                [b'#', ..] => Ok(MaybeReasonedDice::Reasoned(program, ::core::str::from_utf8(inp).unwrap())),
                _ => Err(ReasonedDiceError::UnknownTrailing),
            },
            Some(_) | None => todo!(),
        }
    } else {
        Ok(MaybeReasonedDice::Unreasoned(program))
    }
}

const ROLL_CAP: u64 = 10000;

#[command]
#[aliases("r")]
async fn roll(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    use ::mice::interp;
    let is_cheap_enough = |program: &::mice::parse::Program| -> bool {
        use ::mice::cost::{cost, Price, AstInterp, mbot::{self, TextFormatOutput}};
        match (cost::<AstInterp, _>(program, ()), cost::<TextFormatOutput<mbot::Default>, _>(program, ())) {
            (Price::Bounded(exec), Price::Bounded(fmt)) => exec <= ROLL_CAP && fmt <= ROLL_CAP,
            _ => false,
        }
    };
    macro_rules! handle_proggy_uwu {
        ($program:expr) => {{
            // Due to temporary lifetime extension, we can't put this directly in the match,
            // since `&mut ThreadRng` is not allowed to cross an `.await` point where a
            // `Send` future is required.
            let res = interp::interpret(&mut ::rand::thread_rng(), $program);
            match res {
                Ok(output) => output,
                Err(interp::InterpError::OverflowPositive) => {
                    return reply(ctx, msg, "sum is too high for `i64`").await
                },
                Err(interp::InterpError::OverflowNegative) => {
                    return reply(ctx, msg, "sum is too low for `i64`").await
                }
            }
        }}
    }
    match reasoned_dice(arg.message()) {
        Ok(MaybeReasonedDice::Reasoned(program, reason)) => if reason.len() > MAX_REPLY_LENGTH {
            reply(ctx, msg, "reason given is too long.").await
        } else {
            if is_cheap_enough(&program) {
                let output = handle_proggy_uwu!(&program);
                let dice_msg = if !program.is_single() {
                    ast_format_smart(&program, &output)
                } else {
                    format!("{}", output.total())
                };
                let response = format!("{} {}", dice_msg, reason);
                if response.len() > MAX_REPLY_LENGTH {
                    reply(ctx, msg, &dice_msg).await?;
                    reply(ctx, msg, reason).await
                } else {
                    reply(ctx, msg, &response).await
                }
            } else {
                reply(ctx, msg, "tried to DOS me.").await
            }
        },
        Ok(MaybeReasonedDice::Unreasoned(program)) => if is_cheap_enough(&program) {
            let output = handle_proggy_uwu!(&program);
            if program.is_single() {
                reply(ctx, msg, &format!("{}", output.total())).await
            } else {
                reply(ctx, msg, &ast_format_smart(&program, &output)).await
            }
        } else {
            reply(ctx, msg, "tried to DOS me.").await
        },
        Err(_) => reply(ctx, msg, "you've specified an invalid dice expression").await,
    }
}

#[command]
async fn pinit(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    match pathfinder_initiative(arg.message().trim()) {
        Ok((_, x)) => reply(ctx, msg, &x).await,
        Err(_) => reply(ctx, msg, "invalid initiative bonus listing.").await,
    }
}

fn roll_fate(bonus: Option<i64>) -> FateResult {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let mut results: [i64; 4] = [0; 4];
    let mut sum = 0;
    (1..4).map(|_| rng.gen_range(-1, 2)).fold(0, |a, x| {
        sum += x;
        results[a] = x;
        a + 1
    });
    FateResult {
        dice: results,
        sum: sum + bonus.or(Some(0)).unwrap(),
        bonus,
    }
}

struct FateResult {
    dice: [i64; 4],
    bonus: Option<i64>,
    sum: i64,
}
use core::fmt;
impl fmt::Display for FateResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for x in &self.dice {
            match x {
                -1 => write!(f, "+ (-1)")?,
                0 => write!(f, "+ (0)")?,
                1 => write!(f, "+ (+1)")?,
                _ => unreachable!(),
            }
            write!(f, " ")?
        }
        if let Some(bonus) = self.bonus {
            write!(f, "+ {} = {}", bonus, self.sum)
        } else {
            write!(f, "= {}", self.sum)
        }
    }
}

#[command]
async fn fate(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    match arg.parse::<i64>() {
        Ok(x) => reply(ctx, msg, &format!("{}", roll_fate(Some(x)))).await,
        Err(_) => {
            if arg.message().is_empty() {
                reply(ctx, msg, &format!("{}", roll_fate(None))).await
            } else {
                reply(ctx, msg, "invalid bonus").await
            }
        }
    }
}

#[cfg(feature = "plotting")]
#[command]
async fn plot(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    {
        macro_rules! timed {
            ($e:expr) => ({
                use ::std::time::Instant;
                let first = Instant::now();
                let result = $e;
                let second = Instant::now();
                dbg!(second - first);
                result
            })
        }
        use dist::Plotter;
        use dist::PreparationError;
        let plot = timed!(Plotter::lock());
        let prepared = dbg!(plot.prep(arg.message()));
        match prepared {
            Ok(prepared) => {
                use ::tokio::sync::Semaphore;
                use ::once_cell::sync::Lazy;
                // TODO: consider using finer grained permit counts,
                // based on the relative cost of constructing a chart.
                // The current thing is a stopgap solution meant to
                // save us from being OOM killed.
                // Additionally, consider reporting that we're under load
                // and possibly canceling a plotting job,
                // when not enough permits are available.
                static REGION: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(1));
                let permit = REGION.acquire().await;
                let image = task::spawn_blocking(move || {
                    timed!(plot.draw(prepared))
                }).await.unwrap().unwrap();
                drop(permit);
                msg.channel_id.send_files(&ctx.http, ::core::iter::once(serenity::http::AttachmentType::Bytes {
                    data: ::std::borrow::Cow::Borrowed(&*image),
                    filename: String::from("plot.png"),
                }), |m| m).await?;
                Ok(())
            },
            Err(PreparationError::InvalidExpression) => {
                reply(ctx, msg, "you've specified an invalid dice expression").await
            },
            Err(PreparationError::TooExpensive) => {
                reply(ctx, msg, "tried to DOS me.").await
            }
        }
    }
}

#[command]
async fn dot(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    match ::mice::parse::parse_expression(arg.message().as_bytes()) {
        Ok((input, (_tokens, program))) if input.is_empty() => {
            let mut dot_ast = String::from("```dot\n");
            dot_ast.push_str(&::mice::viz::make_dot(&program));
            dot_ast.push_str("```");
            reply(ctx, msg, &dot_ast).await
        },
        Ok(_) | Err(_) => reply(ctx, msg, "you've specified an invalid dice expression").await
    }
}

#[command]
async fn goodnight(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "Sleep is for the weak, but goodnight.").await
}

// #[check]
// #[name = "in_dev_server"]
// async fn in_dev_server(
//     _: &Context,
//     msg: &Message,
//     _: &mut Args,
//     opts: &CommandOptions,
// ) -> CheckResult {
//     match msg.guild_id {
//         Some(GuildId(579886740097990657)) | Some(GuildId(695085554940772432)) => {
//             CheckResult::Success
//         }
//         _ => CheckResult::Failure(Reason::Log(format!(
//             "attempted to use {} outside dev server",
//             opts.names[0]
//         ))),
//     }
// }

#[cfg(feature = "bot_commands")]
#[group]
#[commands(
    ping, potatoes, happy, po, to, literal, gargamel, roll, pinit, goodnight, fate, dot
)]
struct Green;

#[cfg(feature = "plotting")]
#[group]
#[commands(plot)]
struct Plotting;

// TODO: generalize this
// a lot.
fn lookup_recipient(name: &str) -> Result<ChannelId, ()> {
    match name {
        "ooc" => Ok(ChannelId(695085555511197719)),
        "monad" => Ok(ChannelId(550726052650156054)),
        "cough" => Ok(ChannelId(732325356673040484)),
        _ => Err(()),
    }
}

#[command]
#[aliases("msg")]
async fn anon_msg(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    use foretry::async_try;
    use thiserror::Error;
    #[derive(Error, Debug)]
    enum InternalError {
        #[error("no channel argument provided")]
        NoChannel,
        #[error("invalid recipient name")]
        InvalidRecipient,
        #[error("empty message")]
        EmptyMessage,
        #[error("failed to send message")]
        FailedMessage,
    }
    let http = ctx.http.clone();
    async_try! {(), InternalError | {
        let channel = lookup_recipient(args.current().ok_or(InternalError::NoChannel)?)
            .map_err(|_| InternalError::InvalidRecipient)?;
        args.advance();
        let message = args.remains().ok_or(InternalError::EmptyMessage)?;
        let instance_name = std::env::var("INSTANCE_MESSAGE_PREFIX").unwrap_or_default();
        channel.say(http, &format!("{}Anon: {}", instance_name, message)).await
            .map_err(|_| InternalError::FailedMessage)?;
        match reply(ctx, msg, "message sent.").await {
            Ok(x) => x,
            Err(e) => log::warn!("couldn't send message: {:?}", e),
        }
    } catch (e) {
        reply(ctx, msg, &format!("{}", e)).await?;
    }};

    Ok(())
}

#[group]
#[commands(anon_msg)]
#[only_in(dm)]
struct DMCommands;

struct Handler;
#[serenity::async_trait]
impl EventHandler for Handler {
    #[allow(unused_variables)]
    async fn ready(&self, ctx: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
        #[cfg(feature = "cli_control")]
        match command_loop(ctx, ready).await {
            Ok(_) => (),
            Err(e) => log::error!("{}", e),
        }
        #[cfg(feature = "control_socket")]
        control_socket::control_loop("/home/jmn/mbot.socket").await;
    }
    #[cfg(feature = "internal_rolls")]
    async fn message(&self, ctx: Context, msg: Message) {
        #[cfg(feature = "internal_rolls")] {
            #[allow(clippy::single_match)]
            match internal_rolls::response_for(&msg.content) {
                Some(x) => match reply(&ctx, &msg, &format!("\n{}", x)).await {
                    Ok(()) => (),
                    Err(e) => log::error!("{}", e),
                },
                None => (),
            }
        }
    }
}

#[cfg(not(feature = "static_token"))]
const TOKEN_NAME: &str = "MBOT_TOKEN";
#[cfg(feature = "static_token")]
const TOKEN: &str = env!("MBOT_TOKEN");

use once_cell::sync::OnceCell;
static SHARDS: OnceCell<Arc<Mutex<ShardManager>>> = OnceCell::new();

/// Perform a clean shutdown of the process,
/// closing up outside handles and such before doing so.
#[cfg(any(feature = "cli_control", feature = "control_socket"))]
pub(crate) async fn shutdown() {
    println!("Shutting down...");
    if let Some(shards) = SHARDS.get() {
        shards.lock().await.shutdown_all().await
    }
    std::process::exit(0);
}

#[tokio::main]
async fn main() {
    env_logger::init();
    #[cfg(not(feature = "static_token"))]
    let token = std::env::var(TOKEN_NAME)
        .unwrap_or_else(|_| panic!("Expected evironment variable: {}", TOKEN_NAME));
    #[cfg(feature = "static_token")]
    let token = TOKEN;

    #[cfg(feature = "bot_commands")]
    let framework = {
        let f = StandardFramework::new()
            .configure(|c| c.prefix("!"))
            .group(&GREEN_GROUP)
            .group(&DMCOMMANDS_GROUP);
        #[cfg(feature = "maddie_tools")]
        let f = f.group(&masks::MADDIETOOLS_GROUP);
        #[cfg(feature = "plotting")]
        let f = f.group(&PLOTTING_GROUP);
        f
    };
    let client = Client::builder(&token).event_handler(Handler);

    #[cfg(feature = "bot_commands")]
    let client = client.framework(framework);
    #[cfg(not(feature = "bot_commands"))]
    let client = client.framework(StandardFramework::new());

    let mut client = client.await.expect("Error starting client.");
    // We store this because we need it to shut everything down.
    SHARDS.set(client.shard_manager.clone()).expect("error setting up shutdown handler");

    if let Err(reason) = client.start().await {
        println!("Client error {:#?}", reason);
    }
}
