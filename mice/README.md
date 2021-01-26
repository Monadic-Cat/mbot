[![docs.rs](https://docs.rs/mice/badge.svg)](https://docs.rs/mice/)

# mice, messing with dice

This is a simple crate for evaluating dice expressions.
It will receive features as I desire, or as are contributed
if I happen to be so lucky.

Without further ado, here's some usage:

```rust
use mice::roll;

println!("{}", roll("2d6 + 3")?);

println!("{}", roll("2d6 + 3")?.total());

let result = roll("2d6 + 3")?;
println!("{}\n{}", result, result.total());
```

The parser accepts an arbitrary number of terms in a dice expression.
```rust
use mice::roll;
println!("{}", roll("9d8 + 4d2 - 5 - 8d7")?);
```

Panicking from this crate is considered a serious bug.
Please  submit an Issue on this project's GitHub repo
if it manages to do that, and please describe the usage
that it panicked from.

There is one known unavoidable panic that affects this crate.
Out of memory. If there isn't enough memory for an allocation,
`mice` *will* panic. I hope you understand.

To avoid this fate when exposing `mice` to potentially untrusted
users, there exists the `util::roll_capped` function.
If more control is required, use `expose::tuple_vec`
in combination with the `builder::RollBuilder::with_tuples` method.
Both of these things are reexported by the `prelude` module.
