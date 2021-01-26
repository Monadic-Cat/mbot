use crate::parse::ParseError;
// use std::error::Error as StdError;
use std::ops::Neg;
use thiserror::Error;

/// Most general mice error type. Exported as `MiceError` in the prelude.
#[derive(Debug, Clone, Copy, Error)]
pub enum Error {
    /// This indicates the usage of a die with <= 0 sides
    #[error("Invalid die")]
    InvalidDie,
    /// The sum of terms is greater than what an `i64` can hold
    #[error("sum is too high for `i64`")]
    OverflowPositive(#[from] crate::OverflowPositive),
    /// The sum of terms is lower than what an `i64` can hold
    #[error("sum is too low for `i64`")]
    OverflowNegative(#[from] crate::OverflowNegative),
    /// The expression evaluated isn't a valid dice expression
    #[error("you've specified an invalid dice expression")]
    InvalidExpression(#[from] ParseError),
}

macro_rules! impl_zst_neg {
    ($in:ty => $out:path) => {
        impl Neg for $in {
            type Output = $out;
            fn neg(self) -> Self::Output {
                $out
            }
        }
    }
}
impl_zst_neg!(crate::OverflowPositive => crate::OverflowNegative);
impl_zst_neg!(crate::OverflowNegative => crate::OverflowPositive);
impl Neg for Error {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Error::OverflowPositive(x) => Error::OverflowNegative(-x),
            Error::OverflowNegative(x) => Error::OverflowPositive(-x),
            x => x,
        }
    }
}

pub(crate) enum MyResult<T, E> {
    Ok(T),
    Err(E),
}

impl<T: Neg<Output = T>, E: Neg<Output = E>> Neg for MyResult<T, E> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            MyResult::Ok(x) => MyResult::Ok(-x),
            MyResult::Err(x) => MyResult::Err(-x),
        }
    }
}
impl<T, E> From<MyResult<T, E>> for Result<T, E> {
    fn from(r: MyResult<T, E>) -> Result<T, E> {
        match r {
            MyResult::Ok(x) => Ok(x),
            MyResult::Err(x) => Err(x),
        }
    }
}
impl<T, J, E, R> From<Result<T, E>> for MyResult<J, R>
where
    T: Into<J>,
    E: Into<R>,
{
    fn from(r: Result<T, E>) -> MyResult<J, R> {
        match r {
            Ok(x) => MyResult::Ok(x.into()),
            Err(x) => MyResult::Err(x.into()),
        }
    }
}
