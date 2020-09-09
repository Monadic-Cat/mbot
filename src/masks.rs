use ::serenity::framework::standard::{macros::{command, group}, CommandResult};
use ::serenity::client::Context;
use ::serenity::model::channel::Message;
use crate::reply;

include!(concat!(env!("OUT_DIR"), "/maddie.rs"));
