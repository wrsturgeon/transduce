# `transduce`: Isomorphic Parsing
## Your code should look like what it parses.

See this example from `lib.rs`:
```rust
let parser = exact('(') >> verbatim() << exact(')') << end();
let input = "(*)";
assert_eq!(
    parser.parse(input.chars()),
    Ok('*'),
);
// Or, equivalently:
assert_eq!(
    parenthesized(verbatim()).parse(input.chars()),
    Ok('*'),
);
```
This does exactly what it looks like it does.

Equivalently,
```rust
assert_eq!(parenthesized(verbatim()).parse(rawstr.chars()), Ok('*'))
```

### Pretty-printed errors
The `parse` method automatically locates the error (even for user-defined parsers) and prints out gorgeous, colored Rust-style errors:
```rust
(exact(b'?') >> exact(b'?')).parse_or_panic(b"???");
```
```
...| Error while parsing:
 1 | ???
   |   ^ Unparsed input remains after parsing what should have been everything
```
&nbsp;
```rust
(verbatim() & verbatim() & verbatim() & verbatim()).parse_or_panic(b"???");
```
```
...| Error while parsing:
 1 | ???
   |    ^ Reached end of input but expected an item
```
&nbsp;
```rust
(verbatim() << exact(b'!')).parse_or_panic(b"???")
```
```
...| Error while parsing:
 1 | ???
   |  ^ Expected 33 but found 63
```

### Thanks

Huge shoutout to UPenn's CIS 194 and Haskell's higher-order parsing libraries I learned in 194.

### Future improvements

Removing boxed closures once [this feature](https://github.com/rust-lang/rust/issues/63063) is stabilized.
Already works with the feature-gate enabled: just enable the `nightly` feature on this crate.
I haven't benchmarked yet, but when the name of a type fills almost the entire screen and encodes a needless abstraction with runtime cost, it's gotta go on _moral principle_.
