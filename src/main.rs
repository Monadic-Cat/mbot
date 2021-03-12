#![forbid(unsafe_code)]
use mice::FormatOptions as MiceFormat;
use ::mice::util::ExpressionExt;
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
#[cfg(feature = "turns_db")]
mod turns;
#[cfg(feature = "db")]
mod db;
#[cfg(feature = "maddie_tools")]
mod masks;
#[cfg(feature = "plotting")]
mod dist;

#[cfg(feature = "turns_db")]
use serenity::model::{channel::GuildChannel, id::MessageId, guild::{PartialGuild, Guild}};
use serenity::{
    framework::standard::{
        macros::{check, command, group},
        Args,
        CheckResult, // CommandError,
        CommandOptions,
        CommandResult,
        Reason,
        StandardFramework,
    },
    model::{
        channel::Message,
        gateway::Ready,
        id::{ChannelId, GuildId},
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

fn format_smart(exp: mice::ExpressionResult) -> String {
    let first = exp.format(MiceFormat::new().total_right());
    let second;
    #[allow(clippy::blocks_in_if_conditions)]
    if first.len() < MAX_REPLY_LENGTH {
        first
    } else if {
        second = exp.format(MiceFormat::new().concise().total_right());
        second.len() < MAX_REPLY_LENGTH
    } {
        second
    } else {
        exp.total().to_string()
    }
}

#[derive(Debug)]
enum MaybeReasonedDice {
    Reasoned(mice::parse::Expression),
    Unreasoned(mice::parse::Expression),
}

macro_rules! tript {
    ($tup:expr) => {
        match $tup {
            Ok((i, Ok(x))) => (i, x),
            Ok((i, Err(e))) => return Ok((i, Err(e))),
            Err(e) => return Err(e),
        }
    }
}

fn reasoned_dice(input: &str) -> nom::IResult<&str, Result<MaybeReasonedDice, mice::parse::InvalidDie>> {
    use mice::parse;
    use nom::{branch::alt, bytes::complete::tag, multi::many1};
    let (i, dice) = tript!(parse::dice(input));
    let a = many1(parse::whitespace)(i).and_then(|(i, _)| {
        let _ = alt((tag("to"), tag("for"), tag("because"), tag("#")))(i)?;
        Ok((i, ()))
    });
    match a {
        Ok((i, _)) => Ok((i, Ok(MaybeReasonedDice::Reasoned(dice)))),
        Err(_) => Ok((i, Ok(MaybeReasonedDice::Unreasoned(dice)))),
    }
}

const ROLL_CAP: i64 = 10000;

#[command]
#[aliases("r")]
async fn roll(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    mice::parse::parse_expression(arg.message().as_bytes());
    let dice = match reasoned_dice(arg.message()) {
        Ok((_, Err(e))) => return reply(ctx, msg, &format!("{}", e)).await,
        Ok((i, Ok(x))) => Ok((i, x)),
        Err(e) => Err(e),
    };
    match dice {
        Ok((reason, MaybeReasonedDice::Reasoned(dice))) => {
            if reason.len() > MAX_REPLY_LENGTH {
                reply(ctx, msg, "reason given is too long.").await
            } else {
                let res = if !dice.exceeds_cap(ROLL_CAP) {
                    dice.roll()
                } else {
                    return reply(ctx, msg, "tried to DOS me.").await
                };
                match res {
                    Ok(x) => {
                        let dice_msg = format_smart(x);
                        let resp = format!("{} {}", dice_msg, reason);
                        if resp.len() > MAX_REPLY_LENGTH {
                            reply(ctx, msg, &dice_msg).await?;
                            reply(ctx, msg, &reason).await
                        } else {
                            reply(ctx, msg, &resp).await
                        }
                    }
                    Err(x) => reply(ctx, msg, &format!("{}", x)).await,
                }
            }
        }
        Ok((post_text, MaybeReasonedDice::Unreasoned(dice))) => {
            if post_text.trim() != "" {
                reply(ctx, msg, "you've specified an invalid dice expression").await
            } else {
                let res = if !dice.exceeds_cap(ROLL_CAP) {
                    dice.roll()
                } else {
                    return reply(ctx, msg, "tried to DOS me.").await
                };
                match res {
                    Ok(x) => reply(ctx, msg, &format_smart(x)).await,
                    Err(x) => reply(ctx, msg, &format!("{}", x)).await,
                }
            }
        }
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
    match ::mice::parse::dice(arg.message()) {
        Ok((input, Ok(expression))) if input.is_empty() => {
            // TODO: rate limiting
            if !expression.exceeds_cap(200) {
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
                    dist::draw(&expression, arg.message()).unwrap()
                }).await.expect("plotting doesn't panic");
                drop(permit);
                msg.channel_id.send_files(&ctx.http, ::core::iter::once(serenity::http::AttachmentType::Bytes {
                    data: ::std::borrow::Cow::Borrowed(&*image),
                    filename: String::from("plot.png"),
                }), |m| m).await?;
                Ok(())
            } else {
                reply(ctx, msg, "tried to DOS me.").await
            }
        },
        _ => reply(ctx, msg, "you've specified an invalid dice expression").await
    }
}

#[command]
async fn goodnight(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "Sleep is for the weak, but goodnight.").await
}

#[check]
#[name = "in_dev_server"]
async fn in_dev_server(
    _: &Context,
    msg: &Message,
    _: &mut Args,
    opts: &CommandOptions,
) -> CheckResult {
    match msg.guild_id {
        Some(GuildId(579886740097990657)) | Some(GuildId(695085554940772432)) => {
            CheckResult::Success
        }
        _ => CheckResult::Failure(Reason::Log(format!(
            "attempted to use {} outside dev server",
            opts.names[0]
        ))),
    }
}

#[cfg(feature = "bot_commands")]
#[group]
#[commands(
    ping, potatoes, happy, po, to, literal, gargamel, roll, pinit, goodnight, fate
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
    #[cfg(any(feature = "internal_rolls", feature = "turns_db"))]
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
        // for turn tracking
        #[cfg(feature = "turns_db")] {
            let guild_id = msg.guild_id.unwrap().0 as i64;
            match turns::attempt_turn(
                msg.author.id.0 as i64,
                msg.channel_id.0 as i64,
                msg.timestamp.into(),
            ).await
            {
                Ok(_) => (),
                Err(e @ turns::TurnError::NotInGame(_))
                    | Err(e @ turns::TurnError::NotInRound(_))
                    | Err(e @ turns::TurnError::WrongTurn(_)) => match e.notify_channel() {
                        Some(c) => match c
                            .say(ctx.http, format!("{}: {}", msg.author.mention(), e))
                            .await
                        {
                            Ok(_) => (),
                            Err(e2) => log::error!("{} AFTER {}", e2, e),
                        },
                        None => log::error!("{}", e),
                    },
                // This isn't really an error.
                // It's just someone posting in a channel we see that
                // has no games associated with it.
                // Therefore, do no response and no logging.
                Err(turns::TurnError::NoGameHere) => (),
                // TODO: suppress SQLx errors from when we don't find a server/game/channel
                Err(turns::TurnError::SqlxError(e)) => log::error!("sqlx: {}", e),
            }
        }
    }
    // for turn tracking
    #[cfg(feature = "turns_db")]
    async fn message_delete(&self, ctx: Context, channel_id: ChannelId, deleted_msg_id: MessageId) {
        let pmsgs = match channel_id
            .messages(ctx.http, |b| b.after(deleted_msg_id).limit(1))
            .await
        {
            Ok(x) => x,
            Err(e) => {
                println!("failed to fetch succeeding messages");
                return;
            }
        };
        if pmsgs.len() == 0 {
            unimplemented!("turn rollbacks")
        }
    }
    #[cfg(feature = "turns_db")]
    async fn channel_delete(&self, ctx: Context, channel: &GuildChannel) {
        use foretry::async_try;
        use sqlx::query;
        let channel_id = channel.id.0 as i64;
        async_try! { _, sqlx::Error | {
            let mut transaction = POOL.begin().await?;
            query!("DELETE FROM Channels WHERE ID = ?", channel_id)
                .fetch_one(&mut transaction)
                .await?;
            transaction.commit().await?
        } catch (e) {
            log::error!("sqlx error: {}", e);
        }};
    }
    #[cfg(feature = "turns_db")]
    async fn guild_delete(&self, ctx: Context, incomplete: PartialGuild, full: Option<Guild>) {
        use sqlx::query;
        use foretry::async_try;
        let guild_id = incomplete.id.0 as i64;
        async_try! { _, sqlx::Error | {
            let mut transaction = POOL.begin().await.unwrap();
            query!("DELETE FROM Servers WHERE ID = ?", guild_id)
                .execute(&mut transaction).await.unwrap();
        } catch (e) {
            log::error!("sqlx error: {}", e);
        }}
    }
}

#[cfg(not(feature = "static_token"))]
const TOKEN_NAME: &str = "MBOT_TOKEN";
#[cfg(feature = "static_token")]
const TOKEN: &str = env!("MBOT_TOKEN");

#[cfg(feature = "turns_db")]
use sqlx::SqlitePool;

#[cfg(feature = "turns_db")]
use once_cell::sync::Lazy;

#[cfg(feature = "turns_db")]
use db::POOL;

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
    #[cfg(feature = "db")]
    db::shutdown().await;
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
        #[cfg(feature = "turns_db")]
        {
            f.group(&turns::GMTOOLS_GROUP).group(&turns::PLAYERTOOLS_GROUP)
        }
        #[cfg(not(feature = "turns_db"))]
        {
            f
        }
    };
    let client = Client::builder(&token).event_handler(Handler);

    #[cfg(feature = "bot_commands")]
    let client = client.framework(framework);
    #[cfg(not(feature = "bot_commands"))]
    let client = client.framework(StandardFramework::new());

    let mut client = client.await.expect("Error starting client.");
    // We store this because we need it to shut everything down.
    SHARDS.set(client.shard_manager.clone()).expect("error setting up shutdown handler");

    #[cfg(feature = "db")]
    if let Ok(()) = db::initialize().await {
        println!("sqlite connection initialized");
    } else {
        println!("Failure to initialize database connection.");
        ::std::process::exit(1);
    }

    if let Err(reason) = client.start().await {
        println!("Client error {:#?}", reason);
    }
}
