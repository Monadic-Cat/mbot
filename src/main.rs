#![forbid(unsafe_code)]
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
    msg.channel_id
        .say(&ctx.http, format!("{}, {}", msg.author.mention(), r))?;
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
        &format!("Literal {}", &msg.content["literal".len() + 1..]),
    )
}
#[command]
fn gargamel(ctx: &mut Context, msg: &Message) -> CommandResult {
    reply(ctx, msg, "I think you mean \"Gargalsmell\"!")
}

#[command]
fn roll(ctx: &mut Context, msg: &Message) -> CommandResult {
    // match roll_dice(&msg.content["$roll".len()..]) {
    //     Ok(x) => reply(ctx, msg, &x.to_string()),
    //     Err(x) => reply(ctx, msg, &format!("{}", x)),
    // }
    let expression = &msg.content["$roll".len()..];
    match mice::util::roll_capped(expression, 10000) {
        Ok(x) => reply(ctx, msg, &format!("{}", x)),
        Err(x) => reply(ctx, msg, &format!("{}", x)),
    }
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
    let token =
        std::env::var(TOKEN_NAME).unwrap_or_else(|_| panic!("Expected evironment variable: {}", TOKEN_NAME));
    let mut client = Client::new(&token, Handler).expect("Error starting client.");
    client.with_framework(
        StandardFramework::new()
            .configure(|c| c.prefix("$"))
            .group(&GREEN_GROUP),
    );
    if let Err(reason) = client.start() {
        println!("Client errorL {:#?}", reason);
    }
}
