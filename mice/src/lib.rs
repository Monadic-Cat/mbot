//! # mice, messing with dice
//!
//! This crate is written primarily for my own
//! usage, and will likely obtain extensions related
//! to games that I play.
#![allow(clippy::try_err)]
pub mod parse;
pub mod interp;
pub mod stack;
pub mod viz;
pub mod cost;
pub mod tree;

#[cfg(test)]
mod tests;
