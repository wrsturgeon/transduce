/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Isomorphic parsing: your code should look like what it parses.
//! ## Examples
//! Want to parse something in parentheses? Surround it in parentheses:
//! ```rust
//! use transduce::prelude::*;
//! # fn main() -> transduce::result::Result<()> {
//! (exact('(') >> anything() << exact(')')).parse("(*)".chars())
//! # }
//! ```

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
    clippy::blanket_clippy_restriction_lints,
    clippy::implicit_return,
    clippy::inline_always,
    clippy::mod_module_files,
    clippy::pub_use,
    clippy::question_mark_used,
    clippy::separated_literal_suffix,
    clippy::single_char_lifetime_names
)]

pub mod result;

/// Bring these into scope in each file with `use transduce::prelude::*;`.
pub mod prelude {
    pub use super::{anything, exact, Parser};
}

/// Make a function that _returns a function that_ parses an element in a list.
#[macro_export]
macro_rules! parse_fn {
    (
        $(#[$meta:meta])*
        $($pub:ident)? fn $name:ident$(<$($gen:ident$(: $bound:ident $(+ $bounds:path)*)?),*>)?($($arg:ident: $arg_t:ty),*) -> $I:ty => $O:ty
        |$i:pat_param| $run:expr
    ) => {
        #[inline(always)]
        #[must_use]
        $(#[$meta])*
        $($pub)? fn $name$(<$($gen: 'static + Clone + ::core::fmt::Debug $(+ $bound $(+ $bounds)*)?),*>)?(
            $($arg: $arg_t),*
        // ) -> impl FnOnce(_ParseFn) -> $crate::result::Result<$O> + $lt {
        ) -> Parser<$I, $O> {
            #![allow(clippy::let_underscore_untyped, clippy::question_mark_used, unused_mut)]
            $crate::Parser(Box::new(move |_get_input: &mut dyn FnMut() -> $crate::result::Result<$I>| {
                let $i = _get_input()?;
                Ok($run)
            }))
        }
    };
}

/// Stores a function that can be used later to parse a single item (usually a character) of input.
pub struct Parser<I: 'static + Clone + core::fmt::Debug, O: 'static>(
    #[allow(clippy::type_complexity)]
    // TODO: the fucking *second* `impl trait`-in-aliases rolls out, use it to kill this abomination (see `need-....rs`)
    Box<dyn FnOnce(&mut dyn FnMut() -> result::Result<I>) -> result::Result<O>>,
);

impl<I: 'static + Clone + core::fmt::Debug, O: 'static> Parser<I, O> {
    /// Parse a list of items (usually characters, in which case this "list of characters" is effectively a string).
    /// # Errors
    /// If parsing fails, if we run out of input, or if we have leftover input afterward.
    #[inline(always)]
    pub fn parse<Iter: Iterator<Item = I>>(self, mut is: Iter) -> result::Result<O> {
        let parsed = self.0(&mut || {
            is.next()
                .ok_or_else(|| "Reached end of input while still parsing".to_owned())
        })?;
        is.next()
            .map_or(Ok(parsed), |_| bail!("Leftover input after parsing"))
    }
    /// Construct a new parser that performs an operation and discards its result then performs a second one and returns its result.
    #[inline(always)]
    #[must_use]
    pub fn discard_left<RightO: 'static>(
        // Right-o, laddie! ^^
        self,
        right: Parser<I, RightO>,
    ) -> Parser<I, RightO> {
        #![allow(clippy::question_mark_used)]
        Parser(Box::new(move |get_input| {
            self.0(get_input)?;
            right.0(get_input)
        }))
    }
    /// Construct a new parser that performs an operation and saves its result then performs a second one and discards its result, returning the first.
    #[inline(always)]
    #[must_use]
    pub fn discard_right<RightO: 'static>(
        // Right-o, laddie! ^^
        self,
        right: Parser<I, RightO>,
    ) -> Self {
        #![allow(clippy::question_mark_used)]
        Self(Box::new(move |get_input| {
            let saved = self.0(get_input)?;
            right.0(get_input)?;
            Ok(saved)
        }))
    }
}

impl<I: 'static + Clone + core::fmt::Debug, O, RightO> core::ops::Shr<Parser<I, RightO>>
    for Parser<I, O>
{
    type Output = Parser<I, RightO>;
    #[inline(always)]
    fn shr(self, rhs: Parser<I, RightO>) -> Self::Output {
        self.discard_left(rhs)
    }
}

impl<I: 'static + Clone + core::fmt::Debug, O, RightO> core::ops::Shl<Parser<I, RightO>>
    for Parser<I, O>
{
    type Output = Self;
    #[inline(always)]
    fn shl(self, rhs: Parser<I, RightO>) -> Self::Output {
        self.discard_right(rhs)
    }
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and discard it.
    pub fn exact<I: PartialEq>(expected: I) -> I => ()
    |i| if i != expected { bail!("`exact` failed: expected `{expected:#?}` but found `{i:#?}`") }
}

parse_fn! {
    /// Match any single character and discard it.
    pub fn anything<I>() -> I => ()
    |_| ()
}
