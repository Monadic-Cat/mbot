//! JavaScript bindings for `mice`!
//! Not the entire API is provided yet,
//! but what is *should* satisfy existing
//! guarantees.
use crate::{
    builder::{BuildError, RollBuilder},
    parse::ParseError,
    ExprTuple, ExpressionResult, RollError,
};
use js_sys::{Function, Math::random, Reflect::get};
use rand::rngs::StdRng;
use rand::SeedableRng;
use std::convert::TryInto;
use wasm_bindgen::prelude::*;

impl From<RollError> for JsValue {
    fn from(e: RollError) -> JsValue {
        JsValue::from_str(&format!("{}", e))
    }
}
impl From<ParseError> for JsValue {
    fn from(e: ParseError) -> JsValue {
        JsValue::from_str(&format!("{}", e))
    }
}
impl From<BuildError> for JsValue {
    fn from(e: BuildError) -> JsValue {
        JsValue::from_str(&format!("{}", e))
    }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
impl ExpressionResult {
    pub fn display(&self) -> String {
        format!("{}", self)
    }
}

/// JavaScript binding for `mice::roll`.
// thread_rng isn't supported on WASM
#[wasm_bindgen]
pub fn roll(input: &str) -> Result<ExpressionResult, JsValue> {
    Ok(RollBuilder::new()
        .parse(input)?
        .with_rng(Box::new(StdRng::seed_from_u64(random().to_bits())))
        .into_roll()?
        .roll()?)
}

#[wasm_bindgen]
pub struct Expression {
    exp: crate::parse::Expression,
}

#[wasm_bindgen]
impl Expression {
    /// JavaScript usage:
    /// ```ignore
    /// console.log(parse("2d6 + 3")
    ///             .map((a, b) => [a * 2, b])
    ///             .roll()
    ///             .display())
    /// ```
    /// This doesn't work on numbers too large to fit
    /// with the same precision inside both `f64` and `i64`.
    pub fn map(&self, f: Function) -> Result<Expression, JsValue> {
        let func = |a, b| {
            f.call2(
                &JsValue::null(),
                &JsValue::from_f64(a as f64),
                &JsValue::from_f64(b as f64),
            )
        };
        let mut new_exp = Vec::new();
        for (a, b) in self.exp.iter().map(|x| ExprTuple::from(*x)) {
            let res = func(a, b)?;
            let number = match get(&res, &JsValue::from_f64(0.0))?.as_f64() {
                Some(x) => x,
                None => return Err(JsValue::from_str("Type mismatch")),
            } as i64;
            let size = match get(&res, &JsValue::from_f64(1.0))?.as_f64() {
                Some(x) => x,
                None => return Err(JsValue::from_str("Type mismatch")),
            } as i64;
            new_exp.push((number, size).try_into()?)
        }
        Ok(Expression {
            exp: new_exp,
        })
    }
    pub fn roll(&self) -> Result<ExpressionResult, JsValue> {
        Ok(RollBuilder::new()
            .with_expression(self.exp.clone())
            .with_rng(Box::new(StdRng::seed_from_u64(random().to_bits())))
            .into_roll()?
            .roll()?)
    }
}

// Tuples don't play nicely with JS, apparently.
// I need to provide a better interface.
#[wasm_bindgen]
pub fn parse(input: &str) -> Result<Expression, JsValue> {
    Ok(Expression {
        exp: crate::parse::wrap_dice(input)?,
    })
}

#[wasm_bindgen]
pub fn roll_expression(e: Expression) -> Result<ExpressionResult, JsValue> {
    Ok(e.roll()?)
}
