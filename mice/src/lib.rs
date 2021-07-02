//! # mice, messing with dice
//!
//! This crate is written primarily for my own
//! usage, and will likely obtain extensions related
//! to games that I play.
#![allow(clippy::try_err)]
#[cfg(feature = "thread_rng")]
pub mod parse;
pub mod interp;
pub mod stack;
pub mod viz;
pub mod cost;
pub mod tree;
pub mod mir;

#[cfg(test)]
mod tests;
