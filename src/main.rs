#![forbid(unsafe_code)]
use mice::FormatOptions as MiceFormat;
mod initiative;
use initiative::pathfinder_initiative;
#[cfg(feature = "cli_control")]
mod cmd;
#[cfg(feature = "cli_control")]
use cmd::command_loop;
mod turns;
use serenity::{
    framework::standard::{
        macros::{check, command, group},
        Args, CheckResult, CommandError, CommandOptions, CommandResult, Reason, StandardFramework,
    },
    model::{
        channel::Message,
        gateway::Ready,
        id::{ChannelId, GuildId, MessageId},
    },
    prelude::*,
};

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
    Reasoned(Vec<mice::unstable::parse::Expr>),
    Unreasoned(Vec<mice::unstable::parse::Expr>),
}

fn reasoned_dice(input: &str) -> nom::IResult<&str, MaybeReasonedDice> {
    use mice::unstable::parse;
    use nom::{branch::alt, bytes::complete::tag, multi::many1};
    let (i, dice) = parse::dice(input)?;
    let a = many1(parse::whitespace)(i).and_then(|(i, _)| {
        let _ = alt((tag("to"), tag("for"), tag("because"), tag("#")))(i)?;
        Ok((i, ()))
    });
    match a {
        Ok((i, _)) => Ok((i, MaybeReasonedDice::Reasoned(dice))),
        Err(_) => Ok((i, MaybeReasonedDice::Unreasoned(dice))),
    }
}

const ROLL_CAP: i64 = 10000;

#[command]
#[aliases("r")]
async fn roll(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    use mice::unstable::util::roll_exp_capped;
    match reasoned_dice(arg.message()) {
        Ok((reason, MaybeReasonedDice::Reasoned(dice))) => {
            if reason.len() > MAX_REPLY_LENGTH {
                reply(ctx, msg, "reason given is too long.").await
            } else {
                let res = roll_exp_capped(dice, ROLL_CAP);
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
                let res = roll_exp_capped(dice, ROLL_CAP);
                match res {
                    Ok(x) => reply(ctx, msg, &format!("{}", format_smart(x))).await,
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
        bonus: bonus,
        sum: sum + bonus.or(Some(0)).unwrap(),
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
            if arg.message().len() == 0 {
                reply(ctx, msg, &format!("{}", roll_fate(None))).await
            } else {
                reply(ctx, msg, "invalid bonus").await
            }
        }
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
    if msg.guild_id == Some(GuildId(579886740097990657)) {
        CheckResult::Success
    } else {
        CheckResult::Failure(Reason::Log(format!(
            "attempted to use {} outside dev server",
            opts.names[0]
        )))
    }
}

#[cfg(feature = "bot_commands")]
#[group]
#[commands(
    ping, potatoes, happy, po, literal, gargamel, roll, pinit, goodnight, fate
)]
struct Green;

#[command]
#[only_in(guilds)]
#[aliases("start_game", "sg")]
async fn gm_start_game(ctx: &Context, msg: &Message, _args: Args) -> CommandResult {
    match msg.guild_id {
        Some(x) => match turns::create_game(x) {
            Ok(()) => reply(ctx, msg, "succesfully created the game!").await?,
            Err(_) => reply(ctx, msg, "game already exists.").await?,
        },
        None => (),
    };
    Ok(())
}
#[command]
#[only_in(guilds)] // some `.unwrap()`s in this function rely on this.
#[min_args(2)]
#[max_args(3)]
#[aliases("manage_channel", "mc")]
async fn gm_manage_channel(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let channel: ChannelId = args.single()?;
    let control_state: turns::PostingControls = args.single()?;
    let game_mode: Option<turns::GameMode> = match args.current() {
        Some(x) => {
            let a = Some(x.parse()?);
            args.advance();
            a
        }
        None => None,
    };
    if msg
        .guild(&ctx.cache)
        .await
        .unwrap() // only_in(guilds)
        .read()
        .await
        .channels
        .contains_key(&channel)
    {
        match turns::manage_channel(msg.guild_id.unwrap(), channel, control_state, game_mode) {
            Ok(x) => reply(ctx, msg, "channel configured").await,
            Err(e) => reply(ctx, msg, &format!("{}", e)).await,
        }
    } else {
        reply(ctx, msg, "no such channel in this server").await
    }
}
#[command]
#[only_in(guilds)]
#[num_args(1)]
#[aliases("add_player", "ap")]
async fn gm_add_player(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let user = args.single()?;
    match turns::add_player(msg.guild_id.unwrap(), user) {
        Ok(()) => reply(ctx, msg, "added player").await,
        Err(e) => reply(ctx, msg, &format!("{}", e)).await,
    }
}
#[command]
#[only_in(guilds)]
#[aliases("extend_turn", "et")]
async fn gm_extend_turn(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("turn extensions")
}
#[command]
#[only_in(guilds)]
#[aliases("round_unordered", "ru")]
async fn gm_round_unordered(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("starting unordered rounds")
}
#[command]
#[only_in(guilds)]
#[aliases("round_ordered", "ro")]
async fn gm_round_ordered(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("starting ordered rounds")
}

// TODO: figure out what kind of controls we'll want for manually skipping turns.
#[command]
#[only_in(guilds)]
#[aliases("skip_turn", "st")]
async fn gm_skip_turn(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("turn skipping")
}

#[command]
#[only_in(guilds)]
#[aliases("set_notification_channel", "snc")]
async fn gm_set_notification_channel(_ctx: &Context, _msg: &Message, _args: Args) -> CommandResult {
    unimplemented!("notification channel")
}

#[command]
#[aliases("skip")]
async fn player_skip_turn(_ctx: &Context, _: &Message, _: Args) -> CommandResult {
    unimplemented!("player turn skipping")
}

#[command]
#[only_in(guilds)]
#[aliases("notify")]
async fn player_notify(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let yes_no: turns::YesNo = args.single()?;
    unimplemented!("player notification");
    reply(ctx, msg, &format!("will notify: {:?}", yes_no)).await
}

#[group]
#[prefix("gm")]
#[commands(
    gm_start_game,
    gm_manage_channel,
    gm_add_player,
    gm_extend_turn,
    gm_round_unordered,
    gm_round_ordered,
    gm_skip_turn,
    gm_set_notification_channel
)]
#[checks(in_dev_server)]
#[allowed_roles("GM")]
struct GMTools;

#[group]
#[commands(player_skip_turn, player_notify)]
#[checks(in_dev_server)]
struct PlayerTools;

struct Handler;
#[serenity::async_trait]
impl EventHandler for Handler {
    #[allow(unused_variables)]
    async fn ready(&self, ctx: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
        #[cfg(feature = "cli_control")]
        command_loop(ctx, ready).await;
    }
    // for turn tracking
    async fn message(&self, ctx: Context, msg: Message) {}
    // for turn tracking
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
}

#[cfg(not(feature = "static_token"))]
const TOKEN_NAME: &str = "MBOT_TOKEN";
#[cfg(feature = "static_token")]
const TOKEN: &str = env!("MBOT_TOKEN");

#[tokio::main]
async fn main() {
    #[cfg(not(feature = "static_token"))]
    let token = std::env::var(TOKEN_NAME)
        .unwrap_or_else(|_| panic!("Expected evironment variable: {}", TOKEN_NAME));
    #[cfg(feature = "static_token")]
    let token = TOKEN;

    #[cfg(feature = "bot_commands")]
    let framework = StandardFramework::new()
        .configure(|c| c.prefix("!"))
        .group(&GREEN_GROUP)
        .group(&GMTOOLS_GROUP)
        .group(&PLAYERTOOLS_GROUP);
    let client = Client::new(&token).event_handler(Handler);

    #[cfg(feature = "bot_commands")]
    let client = client.framework(framework);
    #[cfg(not(feature = "bot_commands"))]
    let client = client.framework(StandardFramework::new());

    let mut client = client.await.expect("Error starting client.");

    if let Err(reason) = client.start().await {
        println!("Client error {:#?}", reason);
    }
}
