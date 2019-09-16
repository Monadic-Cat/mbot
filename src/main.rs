#![forbid(unsafe_code)]
use mice::{util::roll_capped, FormatOptions as MiceFormat};
use serenity::{
    framework::{
        standard::{
            macros::{command, group},
            CommandResult,
        },
        StandardFramework,
    },
    model::{channel::Message, gateway::Ready},
    prelude::*,
};

fn reply(ctx: &mut Context, msg: &Message, r: &str) -> CommandResult {
    let instance_name = match option_env!("INSTANCE_MESSAGE_PREFIX") {
        Some(x) => x,
        None => "",
    };
    msg.channel_id.say(
        &ctx.http,
        format!("{}{} {}", instance_name, msg.author.mention(), r),
    )?;
    Ok(())
}

#[command]
fn ping(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "pong")
}
// Suggested by Lasersquid
#[command]
fn potatoes(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "tatoes")
}
#[command]
fn happy(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "La la la la la la! Sing the happy song!")
}
#[command]
fn po(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "tatoes")
}
#[command]
fn literal(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(
        ctx,
        msg,
        &format!("Literal {}", &msg.content["$literal".len() + 1..]),
    )
}
#[command]
fn gargamel(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "I think you mean \"Gargalsmell\"!")
}

fn roll_smart(exp: &str) -> String {
    match roll_capped(exp, 10000) {
        Ok(x) => {
            let first = x.format(MiceFormat::new().total_right());
            let second;
            if first.len() < 1900 {
                first
            } else if {
                second = x.format(MiceFormat::new().concise().total_right());
                second.len() < 1900
            } {
                second
            } else {
                x.total().to_string()
            }
        }
        Err(x) => format!("{}", x),
    }
}

#[command]
fn roll(ctx: &mut Context, msg: &Message) -> CommandResult {
    let expression = &msg.content["$roll".len()..];
    reply(ctx, msg, &roll_smart(expression))
}

group!({
    name: "green",
    options: {},
    commands: [ping, potatoes, happy, po, literal, gargamel, roll],
});

struct Handler;
impl EventHandler for Handler {
    fn ready(&self, _: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
    }
}

const TOKEN_NAME: &str = "MBOT_TOKEN";
fn main() {
    let token = std::env::var(TOKEN_NAME)
        .unwrap_or_else(|_| panic!("Expected evironment variable: {}", TOKEN_NAME));
    let mut client = Client::new(&token, Handler).expect("Error starting client.");
    client.with_framework(
        StandardFramework::new()
            .configure(|c| c.prefix("!"))
            .group(&GREEN_GROUP),
    );
    if let Err(reason) = client.start() {
        println!("Client error {:#?}", reason);
    }
}
