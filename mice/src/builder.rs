//! A builder for rolling stuff.
//! This API affords more flexibility than the basic one.
//! Specifically, it allows a user to provide their own
//! RNG in the place of the default `ThreadRng`.
//! See [`rand` issue #313](https://github.com/rust-random/rand/issues/313)
//! for why this is sometimes necessary.
//!
//! Here's an example port of the base `roll` function to JS:
//! ```ignore
//! use rand::rngs::StdRng;
//! use rand::SeedableRng;
//! use wasm_bindgen::prelude::*;
//! use js_sys::Math::random;
//! use mice::{ExpressionResult, builder::RollBuilder};
//! #[wasm_bindgen]
//! pub fn roll(input: &str) -> Result<ExpressionResult, JsValue> {
//!     Ok(RollBuilder::new()
//!         .parse(input).unwrap()
//!         .with_rng(Box::new(StdRng::seed_from_u64(random().to_bits())))
//!         .into_roll().unwrap()
//!         .roll().unwrap())
//! }
//! ```
//! Note the `.with_rng` call. Without it, `.into_roll()` would
//! unavoidably panic on trying to construct a `ThreadRng` where
//! the underlying `rand` crate does not support it.
// pub use crate::post::FormatOptions;
use crate::{
    expose::ExprTuple,
    parse::{wrap_dice, Expr, Expression, ParseError, InvalidDie},
    post::EResult,
    roll_expr_iter_with,
};
use std::convert::TryFrom;
use thiserror::Error;

use rand::{thread_rng, RngCore, rngs::ThreadRng};
#[derive(Debug, Error)]
pub enum BuildError {
    #[error("builder given no expression")]
    NoExpression,
}

#[derive(Default)]
pub struct RollBuilder {
    expression: Option<Expression>,
}
impl RollBuilder {
    pub fn new() -> RollBuilder {
        RollBuilder {
            expression: None,
        }
    }
    pub fn parse(mut self, input: &str) -> Result<RollBuilder, ParseError> {
        let expression = wrap_dice(input)?;
        self.expression = Some(expression);
        Ok(self)
    }
    pub fn with_tuples(mut self, tuples: &[ExprTuple]) -> Result<RollBuilder, InvalidDie> {
        let mut expression = Vec::new();
        for x in tuples {
            expression.push(Expr::try_from(*x)?)
        }
        self.expression = Some(Expression::new(expression));
        Ok(self)
    }
    #[allow(dead_code)]
    pub(crate) fn with_expression(mut self, expression: Expression) -> RollBuilder {
        self.expression = Some(expression);
        self
    }
    /// Pay specific attention to this when targeting WASM.
    /// Until thread local storage is supported, the default
    /// RNG will not work- provide your own.
    #[allow(dead_code)]
    pub fn with_rng<R: RngCore>(self, rng: R) -> RollBuilderWithRng<R> {
        RollBuilderWithRng {
            generator: rng,
            expression: self.expression,
        }
    }
    /// `into_roll()` can only be used without specifying an RNG
    /// if the default `thread_rng` is supported.
    #[cfg(feature = "thread_rng")]
    pub fn into_roll(self) -> Result<Roll<ThreadRng>, BuildError> {
        Ok(Roll {
            expression: self.expression.ok_or(BuildError::NoExpression)?,
            generator: thread_rng(),
        })
    }
}

pub struct RollBuilderWithRng<R: RngCore> {
    expression: Option<Expression>,
    generator: R,
}
impl<R: RngCore> RollBuilderWithRng<R> {
    pub fn into_roll(self) -> Result<Roll<R>, BuildError> {
        Ok(Roll {
            expression: self.expression.ok_or(BuildError::NoExpression)?,
            generator: self.generator,
        })
    }

    // DUPLICATED CODE:
    pub fn parse(mut self, input: &str) -> Result<Self, ParseError> {
        let expression = wrap_dice(input)?;
        self.expression = Some(expression);
        Ok(self)
    }
    pub fn with_tuples(mut self, tuples: &[ExprTuple]) -> Result<Self, InvalidDie> {
        let mut expression = Vec::new();
        for x in tuples {
            expression.push(Expr::try_from(*x)?)
        }
        self.expression = Some(Expression::new(expression));
        Ok(self)
    }
    #[allow(dead_code)]
    pub(crate) fn with_expression(mut self, expression: Expression) -> Self {
        self.expression = Some(expression);
        self
    }
}

pub struct Roll<R: RngCore> {
    expression: Expression,
    generator: R,
}

impl<R: RngCore> Roll<R> {
    pub fn roll(&mut self) -> EResult {
        roll_expr_iter_with(&mut self.generator, self.expression.iter().copied())
    }
    // /// Proposed public API
    // /// For the purpose of performance, discard all information
    // /// unnecessary for the specified format.
    // pub(crate) fn slim_roll(&mut self, _formatting: FormatOptions) -> EResult {
    //     self.roll()
    // }
}

// /// A roll that has been preemptively verified to be safe,
// /// and thus requires no internal bounds checks.
// /// The space of safe rolls is smaller than the space
// /// of unsafe but still potentially valid rolls.
// /// Still, `i64`s are large enough that this isn't likely
// /// a concern for dice rolling.
// /// Since obtaining this performs the same checks as using
// /// a `Roll`, it is only worth doing if the same expression
// /// is going to be used more than once.
// struct SafeRoll {
//     expression: Expression,
//     generator: Box<dyn RngCore>,
// }
// impl SafeRoll {
//     pub fn roll(&mut self) -> ExpressionResult {

//     }
// }
