/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Common functions to drop in instead of reinventing the wheel.

use crate::{bail, Parser};
use core::fmt::Debug;

#[cfg(feature = "nightly")]
#[allow(clippy::wildcard_imports)]
use nightly_only::*;
#[cfg(feature = "nightly")]
/// Imports necessary only with nightly features.
mod nightly_only {
    pub use crate::result;
    pub use core::iter::Peekable;
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and discard it.
    pub fn exact<I: 'static + PartialEq + Debug>(expect: I) -> (I => ()) {
        Parser::new(move |stream| match stream.next() {
            None => bail!("Reached end of input while still parsing"),
            Some(actual) => {
                if actual == expect {
                    Ok(())
                } else {
                    bail!("`exact` failed: expected `{expect:#?}` but found `{actual:#?}`")
                }
            }
        })
    }
}

parse_fn! {
    /// Match any single item and return it.
    pub fn anything<I>() -> (I => I) {
        Parser::new(|stream| stream.next().ok_or_else(|| "Reached end of input while still parsing".to_owned()))
    }
}

#[cfg(feature = "nightly")]
/// Match an expression in parentheses.
#[inline(always)]
#[must_use]
pub fn parenthesized<
    Output,
    Stream: Iterator<Item = char>,
    Call: FnOnce(&mut Peekable<Stream>) -> result::Result<Output>,
>(
    p: Parser<char, Output, Stream, Call>,
) -> Parser<char, Output, Stream, impl FnOnce(&mut Peekable<Stream>) -> result::Result<Output>> {
    exact('(') >> p << exact(')')
}

#[cfg(not(feature = "nightly"))]
/// Match an expression in parentheses.
#[inline(always)]
#[must_use]
pub fn parenthesized<Output, Stream: Iterator<Item = char>>(
    p: Parser<char, Output, Stream>,
) -> Parser<char, Output, Stream> {
    exact('(') >> p << exact(')')
}

#[cfg(feature = "nightly")]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn skip_while<Input, Predicate: Fn(&Input) -> bool, Stream: Iterator<Item = Input>>(
    pred: Predicate,
) -> Parser<Input, (), Stream, impl FnOnce(&mut Peekable<Stream>) -> result::Result> {
    Parser::new(move |stream| {
        while pred(
            stream
                .peek()
                .ok_or_else(|| "Reached end of input while still parsing".to_owned())?,
        ) {
            stream.next();
        }
        Ok(())
    })
}

#[cfg(not(feature = "nightly"))]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn skip_while<
    Input,
    Predicate: 'static + Fn(&Input) -> bool,
    Stream: Iterator<Item = Input>,
>(
    pred: Predicate,
) -> Parser<Input, (), Stream> {
    Parser::new(move |stream| {
        while pred(
            stream
                .peek()
                .ok_or_else(|| "Reached end of input while still parsing".to_owned())?,
        ) {
            stream.next();
        }
        Ok(())
    })
}

parse_fn! {
    /// Zero or more whitespace characters.
    pub fn whitespace() -> (char => ()) {
        skip_while(|c| matches!(*c, ' ' | '\t' | '\r' | '\n'))
    }
}
