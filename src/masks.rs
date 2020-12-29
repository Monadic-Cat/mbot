//! Masks game specific tools.

use ::serenity::framework::standard::{macros::{command, group}, CommandResult, Args};
use ::serenity::client::Context;
use ::serenity::model::{channel::Message, prelude::ChannelId};
use crate::reply;

async fn move_without_roll_internal(ctx: &Context, id: ChannelId, author: &str, phrase: &str, desc: &str, capital: &str) -> CommandResult {
    let author_line = format!("{} {}", author, phrase);
    let description = format!("**Description**\n{}", desc);
    id.send_message(&ctx.http, |m| m.embed(|e| {
        e.description(description).author(|a| a.name(author_line)).title(capital)
    })).await?;

    Ok(())
}

async fn move_without_roll(ctx: &Context, msg: &Message,
                           phrase: &str, desc: &str, capital: &str) -> CommandResult {
    let id = msg.channel_id;
    move_without_roll_internal(ctx, id, &msg.author.name, phrase, desc, capital).await
}

async fn move_with_roll(ctx: &Context, msg: &Message, mut args: Args,
                        phrase: &str, desc: &str, capital: &str) -> CommandResult {
    let id = msg.channel_id;
    let author_line = format!("{} {}", msg.author.name, phrase);
    let label: i8 = match args.current() {
        Some("info") => {
            return move_without_roll_internal(ctx, id, &msg.author.name, phrase, desc, capital).await;
        },
        Some(label) => match label.parse() {
            Ok(x) => x,
            Err(_) => {
                return reply(ctx, msg, "invalid value for label").await;
            },
        },
        None => 0,
    };
    args.advance();
    let (r1, r2) = {
        use ::rand::Rng;
        let mut rng = ::rand::thread_rng();
        (rng.gen_range(1, 7), rng.gen_range(1, 7))
    };
    let rstr = format!("Dice **{}** + **{}**, Label **{}**", r1, r2, label);
    let rtot = r1 + r2 + label as i16;
    let description = format!("**Description**\n{}", desc);
    id.send_message(&ctx.http, |m| m.embed(|e| {
        e.description(description).author(|a| a.name(author_line)).title(capital)
            .field("Calculation", rstr, false).field("Result", rtot, false)
    })).await?;

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/maddie.rs"));
