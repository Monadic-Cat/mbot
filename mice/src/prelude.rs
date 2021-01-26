//! Re-export of primary useful things in `mice`, so it's
//! easier to get started using it. Just `use mice::prelude::*`
//! and you're off to the races!

pub use crate::{builder::RollBuilder, Error as MiceError, FormatOptions as MiceFormat};

#[cfg(feature = "thread_rng")]
pub use crate::{
    expose::{roll_tuples, tuple_vec},
    roll,
};
