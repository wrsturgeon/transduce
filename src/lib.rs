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
    ffi_unwind_calls,
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
    clippy::mod_module_files,
    clippy::question_mark_used,
    clippy::separated_literal_suffix
)]

use core::marker::PhantomData;

use alloc::string::String;

extern crate alloc;

pub mod base;
pub mod print_error;

// FIXME
/*
#[cfg(test)]
mod test;
*/

/// Parsing error: both a message and where it was, via accepting the length of input _after_ the error.
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    /// Message helpfully describing this error.
    pub message: String,
    /// Length of input _after_ this error, or `None` if we ran out of input (would be -1).
    pub etc: Option<usize>,
}

/// Immediately return with an error. The second argument requests the slice of input _after_ this error, so we can deduce where it happened.
#[macro_export]
macro_rules! bail {
    ($fmsg:expr, $etc:expr) => {
        return Err(ParseError {
            message: ::alloc::format!($fmsg),
            etc: Some(($etc).len()),
        })
    };
}

/// Parse a slice of `Input`s into an `Output`.
pub trait Parse<'input, 'output> {
    /// Raw type, of which we parse a slice. E.g. `u8` if we want to parse a string of bytes, not `&[u8]`: the slice is implied.
    type Input: print_error::PrintError;

    /// Output of parsing a slice of `Input`s.
    type Output: 'output;

    /// Parse exactly one item of input; don't continue without further instruction.
    /// # Errors
    /// When parsing fails.
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError>;
}

/// Wrapper around a parser to work with the orphan rule.
#[derive(Debug)]
pub struct Parser<'input, 'output, Inside: Parse<'input, 'output>>(
    Inside,
    PhantomData<&'input ()>,
    PhantomData<&'output ()>,
);
impl<'input, 'output, Inside: Parse<'input, 'output>> Parser<'input, 'output, Inside> {
    /// Create a Parser without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    pub const fn new(inside: Inside) -> Self {
        Self(inside, PhantomData, PhantomData)
    }
    /// Parse exactly one item of input; don't continue without further instruction.
    /// # Errors
    /// When parsing fails.
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Inside::Input],
    ) -> Result<(Inside::Output, &'input [Inside::Input]), ParseError> {
        self.0.parse(input)
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse(&self, input: &'input [Inside::Input]) -> Result<Inside::Output, String> {
        let (parsed, etc) = match self.partial(input) {
            Ok(ok) => ok,
            Err(err) => {
                return Err(<Inside::Input as print_error::PrintError>::pretty_error(
                    err.message,
                    input,
                    if let Some(i) = err.etc {
                        match i.checked_add(1).map(|ii| input.len().checked_sub(ii)) {
                            Some(Some(s)) => Some(s),
                            _ => return Err(String::from("Internal parsing error: `parse` received an `etc` greater than the slice itself")),
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
                String::from("Unparsed input remains after parsing what should have been everything"),
                input,
                match input.len().checked_sub(etc.len()){
                    some @ Some(_) => some,
                    None => return Err(String::from("Internal parsing error: `parse` received an `etc` greater than the slice itself")),
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
    pub const fn discard_left<'routput, Right: Parse<'input, 'routput, Input = Inside::Input>>(
        self,
        right: Parser<'input, 'routput, Right>,
    ) -> Parser<'input, 'routput, DiscardLeft<'input, 'output, 'routput, Inside, Right>> {
        Parser::new(DiscardLeft::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that returns only the first result.
    #[inline(always)]
    #[must_use]
    pub const fn discard_right<'routput, Right: Parse<'input, 'routput, Input = Inside::Input>>(
        self,
        right: Parser<'input, 'routput, Right>,
    ) -> Parser<'input, 'output, DiscardRight<'input, 'output, 'routput, Inside, Right>> {
        Parser::new(DiscardRight::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that returns both results as a tuple.
    #[inline(always)]
    #[must_use]
    pub const fn both<Right: Parse<'input, 'output, Input = Inside::Input>>(
        self,
        right: Parser<'input, 'output, Right>,
    ) -> Parser<'input, 'output, Both<'input, 'output, Inside, Right>> {
        Parser::new(Both::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that returns the first one to succeed, or the second error if neither succeed.
    #[inline(always)]
    #[must_use]
    pub const fn either<
        Right: Parse<'input, 'output, Input = Inside::Input, Output = Inside::Output>,
    >(
        self,
        right: Parser<'input, 'output, Right>,
    ) -> Parser<'input, 'output, Either<'input, 'output, Inside, Right>> {
        Parser::new(Either::new(self, right))
    }

    /// Consume this parser and integrate it into a two-part parser that pipes parsed output into a normal function and returns its output.
    #[inline(always)]
    #[must_use]
    pub const fn pipe<
        'final_output,
        FinalOutput: 'final_output + 'output,
        Right: Fn(Inside::Output) -> Result<FinalOutput, String>,
    >(
        self,
        right: Right,
    ) -> Parser<
        'input,
        'final_output,
        Pipe<'input, 'output, 'final_output, Inside, FinalOutput, Right>,
    > {
        Parser::new(Pipe::new(self, right))
    }
}

/// Perform two actions in order and discard the result of the first, returning the result of the second.
#[derive(Debug)]
pub struct DiscardLeft<
    'input,
    'loutput,
    'routput,
    Left: Parse<'input, 'loutput, Input = Right::Input>,
    Right: Parse<'input, 'routput>,
>(
    Parser<'input, 'loutput, Left>,
    Parser<'input, 'routput, Right>,
    PhantomData<&'loutput ()>,
);
impl<
        'input,
        'loutput,
        'routput,
        Left: Parse<'input, 'loutput, Input = Right::Input>,
        Right: Parse<'input, 'routput>,
    > DiscardLeft<'input, 'loutput, 'routput, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    pub const fn new(
        left: Parser<'input, 'loutput, Left>,
        right: Parser<'input, 'routput, Right>,
    ) -> Self {
        Self(left, right, PhantomData)
    }
}
impl<
        'input,
        'loutput,
        'routput,
        Left: Parse<'input, 'loutput, Input = Right::Input>,
        Right: Parse<'input, 'routput>,
    > Parse<'input, 'routput> for DiscardLeft<'input, 'loutput, 'routput, Left, Right>
{
    type Input = Right::Input;
    type Output = Right::Output;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        self.1.partial(self.0.partial(input)?.1)
    }
}
impl<
        'input,
        'loutput,
        'routput,
        Left: Parse<'input, 'loutput, Input = Right::Input>,
        Right: Parse<'input, 'routput>,
    > ::core::ops::Shr<Parser<'input, 'routput, Right>> for Parser<'input, 'loutput, Left>
{
    type Output = Parser<'input, 'routput, DiscardLeft<'input, 'loutput, 'routput, Left, Right>>;
    #[inline(always)]
    fn shr(self, rhs: Parser<'input, 'routput, Right>) -> Self::Output {
        self.discard_left(rhs)
    }
}

/// Perform two actions in order and discard the result of the second, returning the result of the first.
#[derive(Debug)]
pub struct DiscardRight<
    'input,
    'loutput,
    'routput,
    Left: Parse<'input, 'loutput>,
    Right: Parse<'input, 'routput, Input = Left::Input>,
>(
    Parser<'input, 'loutput, Left>,
    Parser<'input, 'routput, Right>,
    PhantomData<&'loutput ()>,
);
impl<
        'input,
        'loutput,
        'routput,
        Left: Parse<'input, 'loutput>,
        Right: Parse<'input, 'routput, Input = Left::Input>,
    > DiscardRight<'input, 'loutput, 'routput, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    pub const fn new(
        left: Parser<'input, 'loutput, Left>,
        right: Parser<'input, 'routput, Right>,
    ) -> Self {
        Self(left, right, PhantomData)
    }
}
impl<
        'input,
        'loutput,
        'routput,
        Left: Parse<'input, 'loutput>,
        Right: Parse<'input, 'routput, Input = Left::Input>,
    > Parse<'input, 'loutput> for DiscardRight<'input, 'loutput, 'routput, Left, Right>
{
    type Input = Left::Input;
    type Output = Left::Output;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (result, first_etc) = self.0.partial(input)?;
        let etc = self.1.partial(first_etc)?.1;
        Ok((result, etc))
    }
}
impl<
        'input,
        'loutput,
        'routput,
        Left: Parse<'input, 'loutput>,
        Right: Parse<'input, 'routput, Input = Left::Input>,
    > ::core::ops::Shl<Parser<'input, 'routput, Right>> for Parser<'input, 'loutput, Left>
{
    type Output = Parser<'input, 'loutput, DiscardRight<'input, 'loutput, 'routput, Left, Right>>;
    #[inline(always)]
    fn shl(self, rhs: Parser<'input, 'routput, Right>) -> Self::Output {
        self.discard_right(rhs)
    }
}

/// Perform two actions in order and return both of their results as a tuple.
#[derive(Debug)]
pub struct Both<
    'input,
    'output,
    Left: Parse<'input, 'output>,
    Right: Parse<'input, 'output, Input = Left::Input>,
>(
    Parser<'input, 'output, Left>,
    Parser<'input, 'output, Right>,
    PhantomData<&'output ()>,
);
impl<
        'input,
        'output,
        Left: Parse<'input, 'output>,
        Right: Parse<'input, 'output, Input = Left::Input>,
    > Both<'input, 'output, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    pub const fn new(
        left: Parser<'input, 'output, Left>,
        right: Parser<'input, 'output, Right>,
    ) -> Self {
        Self(left, right, PhantomData)
    }
}
impl<
        'input,
        'output,
        Left: Parse<'input, 'output>,
        Right: Parse<'input, 'output, Input = Left::Input>,
    > Parse<'input, 'output> for Both<'input, 'output, Left, Right>
{
    type Input = Left::Input;
    type Output = (Left::Output, Right::Output);
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (left, first_etc) = self.0.partial(input)?;
        let (right, etc) = self.1.partial(first_etc)?;
        Ok(((left, right), etc))
    }
}
impl<
        'input,
        'output,
        Left: Parse<'input, 'output>,
        Right: Parse<'input, 'output, Input = Left::Input>,
    > ::core::ops::BitAnd<Parser<'input, 'output, Right>> for Parser<'input, 'output, Left>
{
    type Output = Parser<'input, 'output, Both<'input, 'output, Left, Right>>;
    #[inline(always)]
    fn bitand(self, rhs: Parser<'input, 'output, Right>) -> Self::Output {
        self.both(rhs)
    }
}

/// Perform two actions in order and return the first one to succeed, or the second error if neither succeed.
#[derive(Debug)]
pub struct Either<
    'input,
    'output,
    Left: Parse<'input, 'output>,
    Right: Parse<'input, 'output, Input = Left::Input, Output = Left::Output>,
>(
    Parser<'input, 'output, Left>,
    Parser<'input, 'output, Right>,
    PhantomData<&'output ()>,
);
impl<
        'input,
        'output,
        Left: Parse<'input, 'output>,
        Right: Parse<'input, 'output, Input = Left::Input, Output = Left::Output>,
    > Either<'input, 'output, Left, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    pub const fn new(
        left: Parser<'input, 'output, Left>,
        right: Parser<'input, 'output, Right>,
    ) -> Self {
        Self(left, right, PhantomData)
    }
}
impl<
        'input,
        'output,
        Left: Parse<'input, 'output>,
        Right: Parse<'input, 'output, Input = Left::Input, Output = Left::Output>,
    > Parse<'input, 'output> for Either<'input, 'output, Left, Right>
{
    type Input = Left::Input;
    type Output = Left::Output;
    #[inline(always)]
    fn parse(
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
        'output,
        Left: Parse<'input, 'output>,
        Right: Parse<'input, 'output, Input = Left::Input, Output = Left::Output>,
    > ::core::ops::BitOr<Parser<'input, 'output, Right>> for Parser<'input, 'output, Left>
{
    type Output = Parser<'input, 'output, Either<'input, 'output, Left, Right>>;
    #[inline(always)]
    fn bitor(self, rhs: Parser<'input, 'output, Right>) -> Self::Output {
        self.either(rhs)
    }
}

/// Perform a parsing action then pipe its output into a non-parsing function.
#[derive(Debug)]
pub struct Pipe<
    'input,
    'output,
    'final_output,
    Left: Parse<'input, 'output>,
    FinalOutput: 'final_output,
    Right: Fn(Left::Output) -> Result<FinalOutput, String>,
>(
    Parser<'input, 'output, Left>,
    Right,
    PhantomData<&'output ()>,
    PhantomData<&'final_output ()>,
);
impl<
        'input,
        'output,
        'final_output,
        Left: Parse<'input, 'output>,
        FinalOutput: 'final_output,
        Right: Fn(Left::Output) -> Result<FinalOutput, String>,
    > Pipe<'input, 'output, 'final_output, Left, FinalOutput, Right>
{
    /// Create an instance without worrying about workarounds like `PhantomData`.
    #[inline(always)]
    pub const fn new(left: Parser<'input, 'output, Left>, right: Right) -> Self {
        Self(left, right, PhantomData, PhantomData)
    }
}
impl<
        'input,
        'output,
        'final_output,
        Left: Parse<'input, 'output>,
        FinalOutput: 'output,
        Right: Fn(Left::Output) -> Result<FinalOutput, String>,
    > Parse<'input, 'final_output>
    for Pipe<'input, 'output, 'final_output, Left, FinalOutput, Right>
{
    type Input = Left::Input;
    type Output = FinalOutput;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (parsed, etc) = self.0.partial(input)?;
        self.1(parsed).map(|k| (k, etc)).map_err(|e| ParseError {
            message: e,
            etc: Some(etc.len()),
        })
    }
}
impl<
        'input,
        'output,
        Left: Parse<'input, 'output>,
        FinalOutput: 'output,
        Right: Fn(Left::Output) -> Result<FinalOutput, String>,
    > ::core::ops::BitXor<Right> for Parser<'input, 'output, Left>
{
    type Output = Parser<'input, 'output, Pipe<'input, 'output, 'output, Left, FinalOutput, Right>>;
    #[inline(always)]
    fn bitxor(self, rhs: Right) -> Self::Output {
        self.pipe(rhs)
    }
}

/// Read a series of characters (actually `u8`s) into this type.
pub trait Read<Input: print_error::PrintError> {
    /// Parser that can translate from a slice of `Input`s to this type.
    type InternalParser<'input>: Parse<'input, 'static, Input = Input, Output = Self>;
    /// Construct a parser that can translate from a slice of `Input`s to this type.
    #[must_use]
    fn parser<'input>() -> Parser<'input, 'static, Self::InternalParser<'input>>;
}
