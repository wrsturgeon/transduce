/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Zero-copy isomorphic parsing: your code should look like what it parses.
//! ## Examples
//! Want to parse something in parentheses? Surround it in parentheses:
//! ```rust
//! use transduce::base::*;
//! # fn main() -> Result<(), String> {
//! let parser = exact(&b'(') >> anything() << exact(&b')') << end();
//! let input = b"(*)";
//! assert_eq!(
//!     parser.parse(input),
//!     Ok(&b'*'), // Reference to the region of input inside parentheses
//! );
//! # Ok(())
//! # }
//! ```

#![no_std]
#![deny(warnings)]
#![warn(
    clippy::all,
    clippy::missing_docs_in_private_items,
    clippy::nursery,
    clippy::pedantic,
    clippy::restriction,
    clippy::cargo,
    elided_lifetimes_in_paths,
    missing_docs,
    rustdoc::all
)]
// https://doc.rust-lang.org/rustc/lints/listing/allowed-by-default.html
#![warn(
    absolute_paths_not_starting_with_crate,
    box_pointers,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    keyword_idents,
    let_underscore_drop,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    non_ascii_idents,
    noop_method_call,
    pointer_structural_match,
    rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns,
    rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    unstable_features,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_macro_rules,
    unused_qualifications,
    unused_results,
    unused_tuple_struct_fields,
    variant_size_differences
)]
#![allow(
    clippy::blanket_clippy_restriction_lints,
    clippy::implicit_return,
    clippy::inline_always,
    clippy::match_ref_pats,
    clippy::mod_module_files,
    clippy::question_mark_used,
    clippy::separated_literal_suffix
)]

#[cfg(any(feature = "alloc", test))]
extern crate alloc;

pub mod base;
pub mod print_error;

#[cfg(test)]
mod test;

use core::marker::PhantomData;

#[cfg(feature = "alloc")]
use alloc::string::String;

/// Heap-allocated when available; predetermined otherwise.
#[cfg(feature = "alloc")]
type Message = String;

/// Heap-allocated when available; predetermined otherwise.
#[cfg(not(feature = "alloc"))]
type Message = &'static str;

/// Parsing error: both a message and where it was, via accepting the length of input _after_ the error.
#[allow(clippy::exhaustive_structs, missing_copy_implementations)]
#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    /// Message helpfully describing this error.
    pub message: Message,
    /// Length of input _after_ this error, or `None` if we ran out of input (would be -1).
    pub not_yet_parsed: Option<usize>,
}

#[cfg(feature = "alloc")]
/// Immediately return with an error. The second argument requests the slice of input _after_ this error, so we can deduce where it happened.
#[macro_export]
macro_rules! bail {
    ($formatted_msg:expr, $plain_msg:expr, $not_yet_parsed:expr $(,)?) => {
        return Err(ParseError {
            message: ::alloc::format!($formatted_msg),
            not_yet_parsed: Some($not_yet_parsed.len()),
        })
    };
}

#[cfg(not(feature = "alloc"))]
/// Immediately return with an error. The second argument requests the slice of input _after_ this error, so we can deduce where it happened.
#[macro_export]
macro_rules! bail {
    ($formatted_msg:expr, $plain_msg:expr, $not_yet_parsed:expr $(,)?) => {
        return Err(ParseError {
            message: concat!(
                $plain_msg,
                " (specifics unavailable without the `alloc` feature)"
            ),
            not_yet_parsed: Some($not_yet_parsed.len()),
        })
    };
}

#[cfg(feature = "alloc")]
/// Heap-allocated custom formatting if `alloc` is enabled; otherwise, use the first, non-heap argument.
#[macro_export]
macro_rules! format_if_alloc {
    ($default:expr, $fmt:tt$(, $($arg:expr),+)?) => {
        format!($fmt$(, $($arg),+)?)
    };
}

#[cfg(not(feature = "alloc"))]
/// Heap-allocated custom formatting if `alloc` is enabled; otherwise, use the first, non-heap argument.
#[macro_export]
macro_rules! format_if_alloc {
    ($default:expr, $fmt:tt$(, $($arg:expr),+)?) => {
        $default
    };
}

#[cfg(feature = "alloc")]
/// Heap-allocated string if `alloc` enabled; otherwise, pointer to compile-time memory.
#[inline(always)]
#[must_use]
fn string_if_alloc(s: &'static str) -> Message {
    String::from(s)
}

#[cfg(not(feature = "alloc"))]
/// Heap-allocated string if `alloc` enabled; otherwise, pointer to compile-time memory.
#[inline(always)]
#[must_use]
const fn string_if_alloc(s: &'static str) -> Message {
    s
}

/// Parse a slice of `Input`s into an `Output`.
pub trait Parse<'input> {
    /// Raw type, of which we parse a slice. E.g. `u8` if we want to parse a string of bytes, not `&[u8]`: the slice is implied.
    type Input: print_error::PrintError;

    /// Output of parsing a slice of `Input`s.
    type Output;

    /// Parse exactly one item of input; don't continue without further instruction.
    /// # Errors
    /// When parsing fails.
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError>;
}

/// Wrapper around a parser to work with the orphan rule.
#[derive(Clone, Debug)]
pub struct Parser<'input, Inside: Parse<'input>>(Inside, PhantomData<&'input ()>);
impl<'input, Inside: Parse<'input>> Parser<'input, Inside> {
    /// Create a Parser without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    #[must_use]
    pub const fn new(inside: Inside) -> Self {
        Self(inside, PhantomData)
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse(&self, input: &'input [Inside::Input]) -> Result<Inside::Output, Message> {
        let (parsed, etc) = match self.partial(input) {
            Ok(ok) => ok,
            Err(err) => {
                return Err(<Inside::Input as print_error::PrintError>::pretty_error(
                    err.message,
                    input,
                    if let Some(i) = err.not_yet_parsed {
                        match input.len().checked_sub(i) {
                            s @ Some(_) => s,
                            None => return Err(string_if_alloc("Internal parsing error: `parse` received an `etc` greater than the slice itself")),
                        }
                    } else {
                        None
                    },
                ))
            }
        };
        if etc.is_empty() {
            Ok(parsed)
        } else {
            Err(<Inside::Input as print_error::PrintError>::pretty_error(
                string_if_alloc("Unparsed input remains after parsing what should have been everything"),
                input,
                match input.len().checked_sub(etc.len()){
                    s @ Some(_) => s,
                    None => return Err(string_if_alloc("Internal parsing error: `parse` received an `etc` greater than the slice itself")),
                },
            ))
        }
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Panics
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    #[must_use]
    pub fn parse_or_panic(&self, slice: &'input [Inside::Input]) -> Inside::Output {
        #![allow(clippy::panic)]
        self.parse(slice).unwrap_or_else(|err| panic!("{}", err))
    }

    /// Consume this parser and integrate it into a two-part parser that returns only the second result.
    #[inline(always)]
    #[must_use]
    pub const fn discard_left<Right: Parse<'input, Input = Inside::Input>>(
        self,
        right: Parser<'input, Right>,
    ) -> Parser<'input, DiscardLeft<'input, Inside, Right>> {
        Parser::new(DiscardLeft::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that returns only the first result.
    #[inline(always)]
    #[must_use]
    pub const fn discard_right<Right: Parse<'input, Input = Inside::Input>>(
        self,
        right: Parser<'input, Right>,
    ) -> Parser<'input, DiscardRight<'input, Inside, Right>> {
        Parser::new(DiscardRight::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that returns both results as a tuple.
    #[inline(always)]
    #[must_use]
    pub const fn both<Right: Parse<'input, Input = Inside::Input>>(
        self,
        right: Parser<'input, Right>,
    ) -> Parser<'input, Both<'input, Inside, Right>> {
        Parser::new(Both::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that returns the first one to succeed, or the second error if neither succeed.
    #[inline(always)]
    #[must_use]
    pub const fn either<Right: Parse<'input, Input = Inside::Input, Output = Inside::Output>>(
        self,
        right: Parser<'input, Right>,
    ) -> Parser<'input, Either<'input, Inside, Right>> {
        Parser::new(Either::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that pipes parsed output into a normal function and returns its output.
    #[inline(always)]
    #[must_use]
    pub const fn pipe<FinalOutput, Right: Fn(Inside::Output) -> Result<FinalOutput, Message>>(
        self,
        right: Right,
    ) -> Parser<'input, Pipe<'input, Inside, FinalOutput, Right>> {
        Parser::new(Pipe::new(self, right))
    }
}
impl<'input, Inside: Parse<'input>> Parse<'input> for Parser<'input, Inside> {
    type Input = Inside::Input;
    type Output = Inside::Output;
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        self.0.partial(input)
    }
}

/// Perform two actions in order and discard the result of the first, returning the result of the second.
#[derive(Debug)]
pub struct DiscardLeft<'input, Left: Parse<'input, Input = Right::Input>, Right: Parse<'input>>(
    Parser<'input, Left>,
    Parser<'input, Right>,
);
impl<'input, Left: Parse<'input, Input = Right::Input>, Right: Parse<'input>>
    DiscardLeft<'input, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    #[must_use]
    pub const fn new(left: Parser<'input, Left>, right: Parser<'input, Right>) -> Self {
        Self(left, right)
    }
}
impl<'input, Left: Parse<'input, Input = Right::Input>, Right: Parse<'input>> Parse<'input>
    for DiscardLeft<'input, Left, Right>
{
    type Input = Right::Input;
    type Output = Right::Output;
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        self.1.partial(self.0.partial(input)?.1)
    }
}
impl<'input, Left: Parse<'input, Input = Right::Input>, Right: Parse<'input>>
    ::core::ops::Shr<Parser<'input, Right>> for Parser<'input, Left>
{
    type Output = Parser<'input, DiscardLeft<'input, Left, Right>>;
    #[inline(always)]
    #[must_use]
    fn shr(self, rhs: Parser<'input, Right>) -> Self::Output {
        self.discard_left(rhs)
    }
}

/// Perform two actions in order and discard the result of the second, returning the result of the first.
#[derive(Debug)]
pub struct DiscardRight<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>>(
    Parser<'input, Left>,
    Parser<'input, Right>,
);
impl<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>>
    DiscardRight<'input, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    #[must_use]
    pub const fn new(left: Parser<'input, Left>, right: Parser<'input, Right>) -> Self {
        Self(left, right)
    }
}
impl<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>> Parse<'input>
    for DiscardRight<'input, Left, Right>
{
    type Input = Left::Input;
    type Output = Left::Output;
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (result, first_etc) = self.0.partial(input)?;
        let etc = self.1.partial(first_etc)?.1;
        Ok((result, etc))
    }
}
impl<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>>
    ::core::ops::Shl<Parser<'input, Right>> for Parser<'input, Left>
{
    type Output = Parser<'input, DiscardRight<'input, Left, Right>>;
    #[inline(always)]
    #[must_use]
    fn shl(self, rhs: Parser<'input, Right>) -> Self::Output {
        self.discard_right(rhs)
    }
}

/// Perform two actions in order and return both of their results as a tuple.
#[derive(Debug)]
pub struct Both<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>>(
    Parser<'input, Left>,
    Parser<'input, Right>,
);
impl<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>>
    Both<'input, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    #[must_use]
    pub const fn new(left: Parser<'input, Left>, right: Parser<'input, Right>) -> Self {
        Self(left, right)
    }
}
impl<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>> Parse<'input>
    for Both<'input, Left, Right>
{
    type Input = Left::Input;
    type Output = (Left::Output, Right::Output);
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (left, first_etc) = self.0.partial(input)?;
        let (right, etc) = self.1.partial(first_etc)?;
        Ok(((left, right), etc))
    }
}
impl<'input, Left: Parse<'input>, Right: Parse<'input, Input = Left::Input>>
    ::core::ops::BitAnd<Parser<'input, Right>> for Parser<'input, Left>
{
    type Output = Parser<'input, Both<'input, Left, Right>>;
    #[inline(always)]
    #[must_use]
    fn bitand(self, rhs: Parser<'input, Right>) -> Self::Output {
        self.both(rhs)
    }
}

/// Perform two actions in order and return the first one to succeed, or the second error if neither succeed.
#[derive(Debug)]
pub struct Either<
    'input,
    Left: Parse<'input>,
    Right: Parse<'input, Input = Left::Input, Output = Left::Output>,
>(Parser<'input, Left>, Parser<'input, Right>);
impl<
        'input,
        Left: Parse<'input>,
        Right: Parse<'input, Input = Left::Input, Output = Left::Output>,
    > Either<'input, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    #[must_use]
    pub const fn new(left: Parser<'input, Left>, right: Parser<'input, Right>) -> Self {
        Self(left, right)
    }
}
impl<
        'input,
        Left: Parse<'input>,
        Right: Parse<'input, Input = Left::Input, Output = Left::Output>,
    > Parse<'input> for Either<'input, Left, Right>
{
    type Input = Left::Input;
    type Output = Left::Output;
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        match self.0.partial(input) {
            ok @ Ok(_) => ok,
            Err(_) => self.1.partial(input),
        }
    }
}
impl<
        'input,
        Left: Parse<'input>,
        Right: Parse<'input, Input = Left::Input, Output = Left::Output>,
    > ::core::ops::BitOr<Parser<'input, Right>> for Parser<'input, Left>
{
    type Output = Parser<'input, Either<'input, Left, Right>>;
    #[inline(always)]
    #[must_use]
    fn bitor(self, rhs: Parser<'input, Right>) -> Self::Output {
        self.either(rhs)
    }
}

/// Perform a parsing action then pipe its output into a non-parsing function.
#[derive(Debug)]
pub struct Pipe<
    'input,
    Left: Parse<'input>,
    FinalOutput,
    Right: Fn(Left::Output) -> Result<FinalOutput, Message>,
>(Parser<'input, Left>, Right);
impl<
        'input,
        Left: Parse<'input>,
        FinalOutput,
        Right: Fn(Left::Output) -> Result<FinalOutput, Message>,
    > Pipe<'input, Left, FinalOutput, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    #[must_use]
    pub const fn new(left: Parser<'input, Left>, right: Right) -> Self {
        Self(left, right)
    }
}
impl<
        'input,
        Left: Parse<'input>,
        FinalOutput,
        Right: Fn(Left::Output) -> Result<FinalOutput, Message>,
    > Parse<'input> for Pipe<'input, Left, FinalOutput, Right>
{
    type Input = Left::Input;
    type Output = FinalOutput;
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (parsed, etc) = self.0.partial(input)?;
        self.1(parsed).map(|k| (k, etc)).map_err(|e| ParseError {
            message: e,
            not_yet_parsed: Some(input.len()),
        })
    }
}
impl<
        'input,
        Left: Parse<'input>,
        FinalOutput,
        Right: Fn(Left::Output) -> Result<FinalOutput, Message>,
    > ::core::ops::BitXor<Right> for Parser<'input, Left>
{
    type Output = Parser<'input, Pipe<'input, Left, FinalOutput, Right>>;
    #[inline(always)]
    #[must_use]
    fn bitxor(self, rhs: Right) -> Self::Output {
        self.pipe(rhs)
    }
}

/// Read a series of items (of generic type `Input`) into this type.
pub trait Read<'input, Input: print_error::PrintError>: Sized {
    /// Parse a list of `Input`s into this type.
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    fn parse(&self, input: &'input [Input]) -> Result<Self, Message>;
}
