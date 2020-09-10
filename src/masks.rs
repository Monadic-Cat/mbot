//! Masks game specific tools.

use ::serenity::framework::standard::{macros::{command, group}, CommandResult, Args};
use ::serenity::client::Context;
use ::serenity::model::channel::Message;
use crate::reply;

async fn move_without_roll(ctx: &Context, msg: &Message,
                           phrase: &str, desc: &str, capital: &str) -> CommandResult {
    let id = msg.channel_id;
    let author_line = format!("{} {}", msg.author.name, phrase);
    let description = format!("**Description**\n{}", desc);
    id.send_message(&ctx.http, |m| m.embed(|e| {
        e.description(description).author(|a| a.name(author_line)).title(capital)
    })).await?;

    Ok(())
}

async fn move_with_roll(ctx: &Context, msg: &Message, mut args: Args,
                        phrase: &str, desc: &str, capital: &str) -> CommandResult {
    let id = msg.channel_id;
    let author_line = format!("{} {}", msg.author.name, phrase);
    let label: i8 = args.single().unwrap_or(0);
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
