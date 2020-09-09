//! Masks game specific tools.

use ::serenity::framework::standard::{macros::{command, group}, CommandResult, Args};
use ::serenity::client::Context;
use ::serenity::model::channel::Message;

include!(concat!(env!("OUT_DIR"), "/maddie.rs"));
