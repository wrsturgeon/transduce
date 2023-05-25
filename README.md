# `transduce`: Isomorphic Parsing
## Your code should look like what it does.

See this example from `lib.rs`:
```rust
(exact('(') >> anything() << exact(')')).parse("(*)".chars())
```
This does exactly what it looks like it does.

Huge shoutout to UPenn's CIS 194 (which I was lucky enough to take as an enrolled student, because apparently the course is famous?!), and Haskell's innovative higher-order parsing libraries, which I learned in 194.

### Points for further improvement

Boxed closures. Pains in the asses. I haven't run any performance comparisons, but when the name of a type fills almost the entire screen and encodes a needless abstraction with runtime cost, it's on _moral principle_ that I oppose it.
See `need-impl-in-alias.rs` for a better design that fails so far only because `impl Trait` can't yet be used in `type =` expressions in stable Rust, and we need it to return a closure.
