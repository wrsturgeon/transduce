# `transduce`: Zero-Copy Isomorphic Parsing
## Your code should look like what it parses.

See this example from `lib.rs`:
```rust
let parser = exact(&b'(') >> verbatim() << exact(&b')');
let input = b"(*)";
assert_eq!(
    parser.parse(input),
    Ok(&b'*'), // Reference to the region of input inside parentheses
);
// Or, equivalently:
assert_eq!(
    parenthesized(verbatim).parse(input),
    Ok(&b'*'),
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

### `no_std`

The crate is entirely `no_std`; a `std` version isn't even necessary.

Furthermore, the `alloc` feature (enabled by default) can be disabled; this entire crate works without heap allocation.
However, some parsers in `base` (those involving vectors whose length is unknown at compile time, e.g. `comma_separated`) will be unavailable.

### Thanks

Huge shoutout to UPenn's CIS 194 and Haskell's higher-order parsing libraries I learned in 194.
