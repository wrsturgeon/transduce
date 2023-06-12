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
//! # fn main() -> Result<(), String> {
//! assert_eq!(
//!     (exact(b'(') >> verbatim() << exact(b')') << end()).parse(b"(*)")?,
//!     b'*',
//! );
//! # Ok(())
//! # }
//! ```

#![cfg_attr(
    feature = "nightly",
    feature(impl_trait_in_assoc_type, return_position_impl_trait_in_trait)
)]
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
        $($pub)? fn $name$(<$($gen$(: $($lt +)? $bound $(+ $bounds)*)?),*>)?(
            $($arg: $arg_t),*
        ) -> $crate::Parser<$Input, $Output, impl ::core::ops::FnOnce(&[$Input]) -> $crate::result::Result<($Output, &[$Input])>> $body
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
        $($pub)? fn $name$(<$($gen$(: $($lt +)? $bound $(+ $bounds)*)?),*>)?(
            $($arg: $arg_t),*
        ) -> $crate::Parser<$Input, $Output> $body
    };
}

pub mod base;
pub mod print_error;
pub mod result;

#[cfg(test)]
mod test;

#[cfg(feature = "nightly")]
/// Parser wrapping a unique templated callable type.
pub struct Parser<
    Input: 'static,
    Output: 'static,
    Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
>(
    Call,
    ::core::marker::PhantomData<Input>,
    ::core::marker::PhantomData<Output>,
);

#[cfg(not(feature = "nightly"))]
/// Parser wrapping a boxed callable type.
pub struct Parser<Input: 'static, Output: 'static>(
    #[allow(clippy::type_complexity)]
    Box<dyn FnOnce(&[Input]) -> result::Result<(Output, &[Input])>>,
    ::core::marker::PhantomData<Input>,
    ::core::marker::PhantomData<Output>,
);

#[cfg(feature = "nightly")]
impl<
        Input: 'static,
        Output: 'static,
        Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
    > Parser<Input, Output, Call>
{
    /// Construct a parser from a function with the correct signature.
    #[inline(always)]
    #[must_use]
    pub const fn new(f: Call) -> Self {
        Self(f, ::core::marker::PhantomData, ::core::marker::PhantomData)
    }

    /// Parse exactly one item of input; don't continue without further instruction.
    /// # Errors
    /// When parsing fails.
    #[inline(always)]
    pub fn once(self, slice: &[Input]) -> result::Result<(Output, &[Input])> {
        self.0(slice)
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse(self, slice: &[Input]) -> ::core::result::Result<Output, String>
    where
        Input: print_error::PrintError,
    {
        let (parsed, etc) = match self.once(slice) {
            Ok(ok) => ok,
            Err(err) => {
                return Err(Input::pretty_error(
                    err.message,
                    slice,
                    if let Some(i) = err.etc {
                        match i.checked_add(1).map(|ii| slice.len().checked_sub(ii)) {
                            Some(Some(s)) => Some(s),
                            _ => return Err("Internal parsing error: `parse` received an `etc` greater than the slice itself".to_owned()),
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
            Err(Input::pretty_error(
                "Unparsed input remains after parsing what should have been everything".to_owned(),
                slice,
                match slice.len().checked_sub(etc.len()){
                    some @ Some(_) => some,
                    None => return Err("Internal parsing error: `parse` received an `etc` greater than the slice itself".to_owned()),
                },
            ))
        }
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Panics
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    #[must_use]
    pub fn parse_or_panic(self, slice: &[Input]) -> Output
    where
        Input: print_error::PrintError,
    {
        #![allow(clippy::panic)]
        self.parse(slice).unwrap_or_else(|err| panic!("{}", err))
    }

    /// Construct a new parser that performs an operation and discards its result then performs a second one and returns its result.
    #[inline(always)]
    #[must_use]
    pub fn discard_left<
        RightOutput,
        RightCall: FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>,
    >(
        self,
        right: Parser<Input, RightOutput, RightCall>,
    ) -> Parser<Input, RightOutput, impl FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>>
    {
        #![allow(clippy::question_mark_used)]
        Parser::new(move |stream| right.once(self.once(stream)?.1))
    }

    /// Construct a new parser that performs an operation and saves its result then performs a second one and discards its result, returning the first.
    #[inline(always)]
    #[must_use]
    pub fn discard_right<
        RightOutput,
        RightCall: FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>,
    >(
        self,
        right: Parser<Input, RightOutput, RightCall>,
    ) -> Parser<Input, Output, impl FnOnce(&[Input]) -> result::Result<(Output, &[Input])>> {
        #![allow(clippy::question_mark_used)]
        Parser::new(move |stream| {
            let (result, etc) = self.once(stream)?;
            Ok((result, right.once(etc)?.1))
        })
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output> Parser<Input, Output> {
    /// Construct a parser from a function with the correct signature.
    #[inline(always)]
    #[must_use]
    pub fn new<F: 'static + FnOnce(&[Input]) -> result::Result<(Output, &[Input])>>(f: F) -> Self {
        Self(
            Box::new(f),
            ::core::marker::PhantomData,
            ::core::marker::PhantomData,
        )
    }

    /// Parse exactly one item of input; don't continue without further instruction.
    /// # Errors
    /// When parsing fails.
    #[inline(always)]
    pub fn once(self, slice: &[Input]) -> result::Result<(Output, &[Input])> {
        self.0(slice)
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse(self, slice: &[Input]) -> ::core::result::Result<Output, String>
    where
        Input: print_error::PrintError,
    {
        let (parsed, etc) = match self.once(slice) {
            Ok(ok) => ok,
            Err(err) => {
                return Err(Input::pretty_error(
                    err.message,
                    slice,
                    if let Some(i) = err.etc {
                        match i.checked_add(1).map(|ii| slice.len().checked_sub(ii)) {
                            Some(Some(s)) => Some(s),
                            _ => return Err("Internal parsing error: `parse` received an `etc` greater than the slice itself".to_owned()),
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
            Err(Input::pretty_error(
                "Unparsed input remains after parsing what should have been everything".to_owned(),
                slice,
                match slice.len().checked_sub(etc.len()){
                    some @ Some(_) => some,
                    None => return Err("Internal parsing error: `parse` received an `etc` greater than the slice itself".to_owned()),
                },
            ))
        }
    }

    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Panics
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    #[must_use]
    pub fn parse_or_panic(self, slice: &[Input]) -> Output
    where
        Input: print_error::PrintError,
    {
        #![allow(clippy::panic)]
        self.parse(slice).unwrap_or_else(|err| panic!("{}", err))
    }

    /// Construct a new parser that performs an operation and discards its result then performs a second one and returns its result.
    #[inline(always)]
    #[must_use]
    pub fn discard_left<RightOutput>(
        self,
        right: Parser<Input, RightOutput>,
    ) -> Parser<Input, RightOutput> {
        #![allow(clippy::question_mark_used)]
        Parser::new(move |stream| right.once(self.once(stream)?.1))
    }

    /// Construct a new parser that performs an operation and saves its result then performs a second one and discards its result, returning the first.
    #[inline(always)]
    #[must_use]
    pub fn discard_right<RightOutput>(self, right: Parser<Input, RightOutput>) -> Self {
        #![allow(clippy::question_mark_used)]
        Self::new(move |stream| {
            let (result, first_etc) = self.once(stream)?;
            let etc = right.once(first_etc)?.1;
            Ok((result, etc))
        })
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
        RightOutput,
        RightCall: FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>,
    > core::ops::Shr<Parser<Input, RightOutput, RightCall>> for Parser<Input, Output, Call>
{
    type Output = Parser<
        Input,
        RightOutput,
        impl FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>,
    >;
    #[inline(always)]
    #[must_use]
    fn shr(self, rhs: Parser<Input, RightOutput, RightCall>) -> Self::Output {
        self.discard_left(rhs)
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, RightOutput> core::ops::Shr<Parser<Input, RightOutput>>
    for Parser<Input, Output>
{
    type Output = Parser<Input, RightOutput>;
    #[inline(always)]
    #[must_use]
    fn shr(self, rhs: Parser<Input, RightOutput>) -> Self::Output {
        self.discard_left(rhs)
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
        RightOutput,
        RightCall: FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>,
    > core::ops::Shl<Parser<Input, RightOutput, RightCall>> for Parser<Input, Output, Call>
{
    type Output =
        Parser<Input, Output, impl FnOnce(&[Input]) -> result::Result<(Output, &[Input])>>;
    #[inline(always)]
    #[must_use]
    fn shl(self, rhs: Parser<Input, RightOutput, RightCall>) -> Self::Output {
        self.discard_right(rhs)
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, RightOutput> core::ops::Shl<Parser<Input, RightOutput>>
    for Parser<Input, Output>
{
    type Output = Self;
    #[inline(always)]
    #[must_use]
    fn shl(self, rhs: Parser<Input, RightOutput>) -> Self::Output {
        self.discard_right(rhs)
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
        RightOutput,
        RightCall: FnOnce(&[Input]) -> result::Result<(RightOutput, &[Input])>,
    > core::ops::BitAnd<Parser<Input, RightOutput, RightCall>> for Parser<Input, Output, Call>
{
    type Output = Parser<
        Input,
        (Output, RightOutput),
        impl FnOnce(&[Input]) -> result::Result<((Output, RightOutput), &[Input])>,
    >;
    #[inline(always)]
    #[must_use]
    fn bitand(self, rhs: Parser<Input, RightOutput, RightCall>) -> Self::Output {
        Parser::new(move |stream| {
            let (left, first_etc) = self.once(stream)?;
            let (right, etc) = rhs.once(first_etc)?;
            Ok(((left, right), etc))
        })
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, RightOutput> core::ops::BitAnd<Parser<Input, RightOutput>>
    for Parser<Input, Output>
{
    type Output = Parser<Input, (Output, RightOutput)>;
    #[inline(always)]
    #[must_use]
    fn bitand(self, rhs: Parser<Input, RightOutput>) -> Self::Output {
        Parser::new(move |stream| {
            let (left, first_etc) = self.once(stream)?;
            let (right, etc) = rhs.once(first_etc)?;
            Ok(((left, right), etc))
        })
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
        RightCall: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
        Lazy: FnOnce() -> Parser<Input, Output, RightCall>,
    > core::ops::BitOr<Lazy> for Parser<Input, Output, Call>
{
    type Output =
        Parser<Input, Output, impl FnOnce(&[Input]) -> result::Result<(Output, &[Input])>>;
    #[inline(always)]
    #[must_use]
    fn bitor(self, rhs: Lazy) -> Self::Output {
        Parser::new(move |stream| match self.once(stream) {
            ok @ Ok(_) => ok,
            Err(_) => rhs().once(stream),
        })
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, Lazy: 'static + FnOnce() -> Self> core::ops::BitOr<Lazy>
    for Parser<Input, Output>
{
    type Output = Self;
    #[inline(always)]
    #[must_use]
    fn bitor(self, rhs: Lazy) -> Self::Output {
        Self::new(move |stream| match self.once(stream) {
            ok @ Ok(_) => ok,
            Err(_) => rhs().once(stream),
        })
    }
}

#[cfg(feature = "nightly")]
impl<
        Input,
        Output,
        Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
        F: FnOnce(Output) -> PostOutput,
        PostOutput: 'static,
    > core::ops::BitXor<F> for Parser<Input, Output, Call>
{
    type Output =
        Parser<Input, PostOutput, impl FnOnce(&[Input]) -> result::Result<(PostOutput, &[Input])>>;
    #[inline(always)]
    #[must_use]
    fn bitxor(self, rhs: F) -> Self::Output {
        Parser::new(move |stream| {
            let (parsed, etc) = self.once(stream)?;
            Ok((rhs(parsed), etc))
        })
    }
}

#[cfg(not(feature = "nightly"))]
impl<Input, Output, F: 'static + FnOnce(Output) -> PostOutput, PostOutput: 'static>
    core::ops::BitXor<F> for Parser<Input, Output>
{
    type Output = Parser<Input, PostOutput>;
    #[inline(always)]
    #[must_use]
    fn bitxor(self, rhs: F) -> Self::Output {
        Parser::new(move |stream| {
            let (parsed, etc) = self.once(stream)?;
            Ok((rhs(parsed), etc))
        })
    }
}

/// Read a series of characters (actually `u8`s) into this type.
pub trait Read: Sized {
    #[cfg(feature = "nightly")]
    /// Create a parser that can read `u8`s into this type.
    #[must_use]
    fn parser() -> Parser<u8, Self, impl FnOnce(&[u8]) -> result::Result<(Self, &[u8])>>;

    #[cfg(not(feature = "nightly"))]
    /// Create a parser that can read `u8`s into this type.
    #[must_use]
    fn parser() -> Parser<u8, Self>;
}
