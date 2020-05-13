#![forbid(unsafe_code)]
use mice::{FormatOptions as MiceFormat};
mod initiative;
use initiative::pathfinder_initiative;
use serenity::{
    framework::standard::{
        macros::{command, group},
        CommandResult, StandardFramework,
        Args,
    },
    model::{channel::Message, gateway::Ready},
    prelude::*,
    async_trait,
};

async fn reply(ctx: &Context, msg: &Message, r: &str) -> CommandResult {
    let instance_name = std::env::var("INSTANCE_MESSAGE_PREFIX").unwrap_or_default();
    msg.channel_id.say(
        &ctx.http,
        format!("{}{} {}", instance_name, msg.author.mention(), r),
    ).await?;
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
    reply(
        ctx,
        msg,
        &format!("Literal {}", arg.message().trim()),
    ).await
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
    use nom::{bytes::complete::tag, multi::many1, branch::alt};
    use mice::unstable::parse;
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
                    },
                    Err(x) => reply(ctx, msg, &format!("{}", x)).await,
                }
            }
        },
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
        },
        Err(_) => { reply(ctx, msg, "you've specified an invalid dice expression").await },
    }
}

#[command]
async fn pinit(ctx: &Context, msg: &Message, arg: Args) -> CommandResult {
    match pathfinder_initiative(arg.message().trim()) {
        Ok((_, x)) => reply(ctx, msg, &x).await,
        Err(_) => reply(ctx, msg, "invalid initiative bonus listing.").await,
    }
}

#[command]
async fn goodnight(ctx: &Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "Sleep is for the weak, but goodnight.").await
}

#[group]
#[commands(ping, potatoes, happy, po, literal, gargamel, roll, pinit)]
struct Green;

struct Handler;
#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, ctx: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
    }
}

const TOKEN_NAME: &str = "MBOT_TOKEN";
#[tokio::main]
async fn main() {
    let token = std::env::var(TOKEN_NAME)
        .unwrap_or_else(|_| panic!("Expected evironment variable: {}", TOKEN_NAME));
    let framework = StandardFramework::new()
        .configure(|c| c.prefix("!"))
        .group(&GREEN_GROUP);
    let client = Client::new(&token).event_handler(Handler);

    let client = client.framework(framework);

    let mut client = client.await.expect("Error starting client.");

    if let Err(reason) = client.start().await {
        println!("Client error {:#?}", reason);
    }
}
