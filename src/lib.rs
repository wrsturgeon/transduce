/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Isomorphic parsing: your code should look like what it parses.
//! ## Examples
//! Want to parse something in parentheses? Surround it in parentheses:
//! ```rust
//! use transduce::base::*;
//! # fn main() {
//! let parser = exact('(') >> anything() << exact(')');
//! let rawstr = "(*)";
//! assert_eq!(parser.parse(rawstr.chars()), Ok('*'));
//! assert_eq!(parenthesized(anything()).parse(rawstr.chars()), Ok('*'));
//! # }
//! ```

#![cfg_attr(feature = "nightly", feature(impl_trait_in_assoc_type))]
#![cfg_attr(feature = "nightly", feature(type_alias_impl_trait))]
#![deny(warnings)]
#![warn(
    clippy::all,
    clippy::missing_docs_in_private_items,
    clippy::nursery,
    clippy::pedantic,
    clippy::restriction,
    clippy::cargo,
    missing_docs,
    rustdoc::all
)]
#![allow(
    clippy::arithmetic_side_effects,
    clippy::blanket_clippy_restriction_lints,
    clippy::implicit_return,
    clippy::inline_always,
    clippy::mod_module_files,
    clippy::pub_use,
    clippy::question_mark_used,
    clippy::separated_literal_suffix,
    clippy::single_char_lifetime_names
)]

#[cfg(feature = "nightly")]
/// Make a function that _returns a wrapper around a function that_ parses an element in a list.
#[macro_export]
macro_rules! parse_fn {
    (
        $(#[$meta:meta])*
        $($pub:ident)? fn $name:ident$(<$($gen:ident$(: $($lt:lifetime +)? $bound:ident $(+ $bounds:path)*)?),*>)?($($arg:ident: $arg_t:ty),*) -> ($Input:ty => $Output:ty)
        $body:block
    ) => {
        #[inline(always)]
        #[must_use]
        $(#[$meta])*
        $($pub)? fn $name<Stream: ::core::iter::Iterator<Item = $Input>, $($($gen$(: $($lt +)? $bound $(+ $bounds)*)?),*)?>(
            $($arg: $arg_t),*
        ) -> $crate::Parser<$Input, $Output, Stream, impl ::core::ops::FnOnce(&mut ::core::iter::Peekable<Stream>) -> $crate::result::Result<$Output>> $body
    };
}

#[cfg(not(feature = "nightly"))]
/// Make a function that _returns a wrapper around a function that_ parses an element in a list.
#[macro_export]
macro_rules! parse_fn {
    (
        $(#[$meta:meta])*
        $($pub:ident)? fn $name:ident$(<$($gen:ident$(: $($lt:lifetime +)? $bound:ident $(+ $bounds:path)*)?),*>)?($($arg:ident: $arg_t:ty),*) -> ($Input:ty => $Output:ty)
        $body:block
    ) => {
        #[inline(always)]
        #[must_use]
        $(#[$meta])*
        $($pub)? fn $name<Stream: ::core::iter::Iterator<Item = $Input>, $($($gen$(: $($lt +)? $bound $(+ $bounds)*)?),*)?>(
            $($arg: $arg_t),*
        ) -> $crate::Parser<$Input, $Output, Stream> $body
    };
}

pub mod base;
pub mod result;

use ::core::{iter::Peekable, marker::PhantomData};

/// Parser wrapping a unique templated function type.
pub struct Parser<
    Input,
    Output: 'static,
    Stream: 'static + Iterator<Item = Input>,
    #[cfg(feature = "nightly")] Call: FnOnce(&mut Peekable<Stream>) -> result::Result<Output>,
>(
    #[cfg(feature = "nightly")] Call,
    #[cfg(not(feature = "nightly"))]
    #[allow(clippy::type_complexity)]
    Box<dyn FnOnce(&mut Peekable<Stream>) -> result::Result<Output>>,
    PhantomData<Input>,
    PhantomData<Output>,
    PhantomData<Stream>,
);

#[cfg(feature = "nightly")]
impl<
        Input,
        Output: 'static,
        Stream: 'static + Iterator<Item = Input>,
        Call: FnOnce(&mut Peekable<Stream>) -> result::Result<Output>,
    > Parser<Input, Output, Stream, Call>
{
    /// Construct a parser from a function with the correct signature.
    #[inline(always)]
    #[must_use]
    pub const fn new(f: Call) -> Self {
        Self(f, PhantomData, PhantomData, PhantomData)
    }
    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse(self, stream: Stream) -> result::Result<Output> {
        let mut iter = stream.peekable();
        let parsed = self.0(&mut iter)?;
        iter.next()
            .map_or(Ok(parsed), |_| bail!("Leftover input after parsing"))
    }
    /// Construct a new parser that performs an operation and discards its result then performs a second one and returns its result.
    #[inline(always)]
    #[must_use]
    pub fn discard_left<
        RightOutput,
        RightCall: FnOnce(&mut Peekable<Stream>) -> result::Result<RightOutput>,
    >(
        self,
        right: Parser<Input, RightOutput, Stream, RightCall>,
    ) -> Parser<
        Input,
        RightOutput,
        Stream,
        impl FnOnce(&mut Peekable<Stream>) -> result::Result<RightOutput>,
    > {
        #![allow(clippy::question_mark_used)]
        Parser::new(move |stream| {
            self.0(stream)?;
            right.0(stream)
        })
    }
    /// Construct a new parser that performs an operation and saves its result then performs a second one and discards its result, returning the first.
    #[inline(always)]
    #[must_use]
    pub fn discard_right<
        RightOutput,
        RightCall: FnOnce(&mut Peekable<Stream>) -> result::Result<RightOutput>,
    >(
        self,
        right: Parser<Input, RightOutput, Stream, RightCall>,
    ) -> Parser<Input, Output, Stream, impl FnOnce(&mut Peekable<Stream>) -> result::Result<Output>>
    {
        #![allow(clippy::question_mark_used)]
        Parser::new(move |stream| {
            let result = self.0(stream)?;
            right.0(stream)?;
            Ok(result)
        })
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output: 'static, Stream: 'static + Iterator<Item = Input>>
    Parser<Input, Output, Stream>
{
    /// Construct a parser from a function with the correct signature.
    #[inline(always)]
    #[must_use]
    pub fn new<F: 'static + FnOnce(&mut Peekable<Stream>) -> result::Result<Output>>(f: F) -> Self {
        Self(Box::new(f), PhantomData, PhantomData, PhantomData)
    }
    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse(self, stream: Stream) -> result::Result<Output> {
        let mut iter = stream.peekable();
        let parsed = self.0(&mut iter)?;
        iter.next()
            .map_or(Ok(parsed), |_| bail!("Leftover input after parsing"))
    }
    /// Construct a new parser that performs an operation and discards its result then performs a second one and returns its result.
    #[inline(always)]
    #[must_use]
    pub fn discard_left<RightOutput: 'static>(
        self,
        right: Parser<Input, RightOutput, Stream>,
    ) -> Parser<Input, RightOutput, Stream> {
        #![allow(clippy::question_mark_used)]
        Parser::new(move |stream| {
            self.0(stream)?;
            right.0(stream)
        })
    }
    /// Construct a new parser that performs an operation and saves its result then performs a second one and discards its result, returning the first.
    #[inline(always)]
    #[must_use]
    pub fn discard_right<RightOutput: 'static>(
        self,
        right: Parser<Input, RightOutput, Stream>,
    ) -> Self {
        #![allow(clippy::question_mark_used)]
        Self::new(move |stream| {
            let result = self.0(stream)?;
            right.0(stream)?;
            Ok(result)
        })
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Stream: Iterator<Item = Input>,
        Call: FnOnce(&mut Peekable<Stream>) -> result::Result<Output>,
        RightOutput,
        RightCall: FnOnce(&mut Peekable<Stream>) -> result::Result<RightOutput>,
    > core::ops::Shr<Parser<Input, RightOutput, Stream, RightCall>>
    for Parser<Input, Output, Stream, Call>
{
    type Output = Parser<
        Input,
        RightOutput,
        Stream,
        impl FnOnce(&mut Peekable<Stream>) -> result::Result<RightOutput>,
    >;
    #[inline(always)]
    #[must_use]
    fn shr(self, rhs: Parser<Input, RightOutput, Stream, RightCall>) -> Self::Output {
        self.discard_left(rhs)
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, Stream: Iterator<Item = Input>, RightOutput>
    core::ops::Shr<Parser<Input, RightOutput, Stream>> for Parser<Input, Output, Stream>
{
    type Output = Parser<Input, RightOutput, Stream>;
    #[inline(always)]
    #[must_use]
    fn shr(self, rhs: Parser<Input, RightOutput, Stream>) -> Self::Output {
        self.discard_left(rhs)
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Stream: Iterator<Item = Input>,
        Call: FnOnce(&mut Peekable<Stream>) -> result::Result<Output>,
        RightOutput,
        RightCall: FnOnce(&mut Peekable<Stream>) -> result::Result<RightOutput>,
    > core::ops::Shl<Parser<Input, RightOutput, Stream, RightCall>>
    for Parser<Input, Output, Stream, Call>
{
    type Output =
        Parser<Input, Output, Stream, impl FnOnce(&mut Peekable<Stream>) -> result::Result<Output>>;
    #[inline(always)]
    #[must_use]
    fn shl(self, rhs: Parser<Input, RightOutput, Stream, RightCall>) -> Self::Output {
        self.discard_right(rhs)
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, Stream: Iterator<Item = Input>, RightOutput>
    core::ops::Shl<Parser<Input, RightOutput, Stream>> for Parser<Input, Output, Stream>
{
    type Output = Self;
    #[inline(always)]
    #[must_use]
    fn shl(self, rhs: Parser<Input, RightOutput, Stream>) -> Self::Output {
        self.discard_right(rhs)
    }
}
