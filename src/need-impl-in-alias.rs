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
//! let parenthesized = |inside| {
//!     exact('(') >> inside << exact(')')
//! };
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
    clippy::separated_literal_suffix,
    clippy::single_char_lifetime_names
)]

mod result;

/// Bring these into scope in each file with `use transduce::prelude::*;`.
pub mod prelude {
    pub use super::{exact, Parser};
}

use core::fmt::Debug;

/// Make a function that _returns a function that_ parses an element in a list.
#[macro_export]
macro_rules! parse_fn {
    (
        $(#[$meta:meta])*
        $($pub:ident)? fn $name:ident<$lt:lifetime $(, $gen:ident$(: $bound:ident $(+ $bounds:path)*)?)*>($($arg:ident: $arg_t:ty),*) -> $I:ty => $O:ty
        |$i:ident| $run:expr
    ) => {
        #[inline(always)]
        $(#[$meta])*
        $($pub)? fn $name<$lt, _ParseFn: Fn() -> $crate::result::Result<&$lt $I> $(, $gen$(: $bound $(+ $bounds)*)?)*>(
            $($arg: $arg_t),*
        // ) -> impl FnOnce(_ParseFn) -> $crate::result::Result<$O> + $lt {
        ) -> Parser<$lt, impl FnOnce(&_ParseFn) -> $crate::result::Result<$O> + $lt, _ParseFn, $I, $O> {
            #![allow(clippy::question_mark_used, unused_mut)]
            $crate::Parser::new(move |_get_input: &_ParseFn| {
                let mut $i = _get_input()?;
                Ok($run)
            })
        }
    };
}

/// Stores a function that can be used later to parse a single item (usually a character) of input.
pub struct Parser<
    'a,
    F: FnOnce(&G) -> result::Result<O>,
    G: FnOnce() -> result::Result<&'a I>,
    I: 'a,
    O,
>(F, core::marker::PhantomData<G>);

impl<'a, F: FnOnce(&G) -> result::Result<O>, G: FnOnce() -> result::Result<&'a I>, I: 'a, O>
    Parser<'a, F, G, I, O>
{
    /// Construct a new parser that thinly wraps a function.
    #[inline(always)]
    pub const fn new(f: F) -> Self {
        Self(f, core::marker::PhantomData)
    }
    /// Construct a new parser that performs an operation and discards its result then performs a second one and returns its result.
    #[inline(always)]
    pub fn discard_left<F2: FnOnce(&G) -> result::Result<O2>, O2>(
        self,
        right: Parser<'a, F2, G, I, O2>,
    ) -> Parser<'a, impl FnOnce(&G) -> result::Result<O2>, G, I, O2> {
        Parser::new(move |get_input| {
            self.0(get_input)?;
            right.0(get_input)
        })
    }
}

impl<
        'a,
        F: FnOnce(&G) -> result::Result<O>,
        G: FnOnce() -> result::Result<&'a I>,
        I: 'a,
        O,
        F2: FnOnce(&G) -> result::Result<O2>,
        O2,
    > core::ops::Shr<Parser<'a, F2, G, I, O2>> for Parser<'a, F, G, I, O>
{
    type Output = Parser<'a, impl FnOnce(&G) -> result::Result<O2>, G, I, O2>;
    #[inline(always)]
    fn shr(self, rhs: Parser<'a, F2, G, I, O2>) -> Self::Output {
        self.discard_left(rhs)
    }
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and discard it.
    pub fn exact<'a, I: PartialEq + Debug>(expected: I) -> I => ()
    |i| if i != &expected { bail!("`exact` failed: expected `{expected:#?}` but found `{i:#?}`") }
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and discard it.
    pub fn exact_ref<'a, I: PartialEq + Debug>(expected: &'a I) -> I => ()
    |i| if i != expected { bail!("`exact` failed: expected `{expected:#?}` but found `{i:#?}`") }
}
