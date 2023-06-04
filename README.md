# `transduce`: Isomorphic Parsing
## Your code should look like what it does.

See this example from `lib.rs`:
```rust
let parser = exact('(') >> anything() << exact(')');
let rawstr = "(*)";
assert_eq!(parser.parse(rawstr.chars()), Ok('*'));
```
This does exactly what it looks like it does.

Equivalently,
```rust
assert_eq!(parenthesized(anything()).parse(rawstr.chars()), Ok('*'))
```

Huge shoutout to UPenn's CIS 194 and Haskell's higher-order parsing libraries I learned in 194.

### Future improvements

Removing boxed closures once [this feature](https://github.com/rust-lang/rust/issues/63063) is stabilized.
Already works with the feature-gate enabled: just enable the `nightly` feature on this crate.
I haven't benchmarked yet, but when the name of a type fills almost the entire screen and encodes a needless abstraction with runtime cost, it's gotta go on _moral principle_.
