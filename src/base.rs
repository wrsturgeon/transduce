/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Common functions to drop in instead of reinventing the wheel.

#![allow(unreachable_code)] // TODO: REMOVE

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
    /// Match the end of a stream.
    pub fn end<I>() -> (I => ()) {
        Parser::new(move |slice| {
            if !slice.is_empty() {
                bail!("Tried to match the end of a stream but found input remaining")
            }
            Ok(((), &[]))
        })
    }
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and discard it.
    pub fn exact<I: 'static + PartialEq + Debug>(expect: I) -> (I => ()) {
        Parser::new(move |slice| match slice.split_first() {
            None => bail!("Reached end of input while still parsing"),
            Some((head, tail)) => {
                if head == &expect {
                    Ok(((), tail))
                } else {
                    bail!("`exact` failed: expected `{expect:#?}` but found `{head:#?}`")
                }
            }
        })
    }
}

parse_fn! {
    /// Match any single item and return it.
    pub fn verbatim<I: Clone>() -> (I => I) {
        Parser::new(move |slice| {
            slice
                .split_first()
                .map(move |(x, etc): (&I, _)| (x.clone(), etc))
                .ok_or_else(|| "`verbatim` reached end of input while still parsing".to_owned())
        })
    }
}

#[cfg(feature = "nightly")]
/// Parse an expression between two (discarded) items.
#[inline(always)]
#[must_use]
pub fn wrapped<
    Input: PartialEq + Debug,
    Output,
    Call: FnOnce(&[Input]) -> result::Result<(Output, &[Input])>,
>(
    before: Input,
    p: Parser<Input, Output, Call>,
    after: Input,
) -> Parser<Input, Output, impl FnOnce(&[Input]) -> result::Result<(Output, &[Input])>> {
    exact(before) >> p << exact(after)
}

#[cfg(not(feature = "nightly"))]
/// Parse an expression between two (discarded) items.
#[inline(always)]
#[must_use]
pub fn wrapped<Input: 'static + PartialEq + Debug, Output>(
    before: Input,
    p: Parser<Input, Output>,
    after: Input,
) -> Parser<Input, Output> {
    exact(before) >> p << exact(after)
}

#[cfg(feature = "nightly")]
/// Parse an expression in parentheses: `(...)`.
#[inline(always)]
#[must_use]
pub fn parenthesized<Output, Call: FnOnce(&[u8]) -> result::Result<(Output, &[u8])>>(
    p: Parser<u8, Output, Call>,
) -> Parser<u8, Output, impl FnOnce(&[u8]) -> result::Result<(Output, &[u8])>> {
    wrapped(b'(', p, b')')
}

#[cfg(not(feature = "nightly"))]
/// Parse an expression in parentheses: `(...)`.
#[inline(always)]
#[must_use]
pub fn parenthesized<Output>(p: Parser<u8, Output>) -> Parser<u8, Output> {
    wrapped(b'(', p, b')')
}

#[cfg(feature = "nightly")]
/// Parse an expression in brackets: `[...]`.
#[inline(always)]
#[must_use]
pub fn bracketed<Output, Call: FnOnce(&[u8]) -> result::Result<(Output, &[u8])>>(
    p: Parser<u8, Output, Call>,
) -> Parser<u8, Output, impl FnOnce(&[u8]) -> result::Result<(Output, &[u8])>> {
    wrapped(b'[', p, b']')
}

#[cfg(not(feature = "nightly"))]
/// Parse an expression in brackets: `[...]`.
#[inline(always)]
#[must_use]
pub fn bracketed<Output>(p: Parser<u8, Output>) -> Parser<u8, Output> {
    wrapped(b'[', p, b']')
}

#[cfg(feature = "nightly")]
/// Parse an expression in curly braces: `{...}`.
#[inline(always)]
#[must_use]
pub fn braced<Output, Call: FnOnce(&[u8]) -> result::Result<(Output, &[u8])>>(
    p: Parser<u8, Output, Call>,
) -> Parser<u8, Output, impl FnOnce(&[u8]) -> result::Result<(Output, &[u8])>> {
    wrapped(b'{', p, b'}')
}

#[cfg(not(feature = "nightly"))]
/// Parse an expression in curly braces: `{...}`.
#[inline(always)]
#[must_use]
pub fn braced<Output>(p: Parser<u8, Output>) -> Parser<u8, Output> {
    wrapped(b'{', p, b'}')
}

#[cfg(feature = "nightly")]
/// Parse an expression in angle brackets: `<...>`.
#[inline(always)]
#[must_use]
pub fn angle_bracketed<Output, Stream, Call: FnOnce(&[u8]) -> result::Result<(Output, &[u8])>>(
    p: Parser<u8, Output, Call>,
) -> Parser<u8, Output, impl FnOnce(&[u8]) -> result::Result<(Output, &[u8])>> {
    wrapped(b'<', p, b'>')
}

#[cfg(not(feature = "nightly"))]
/// Parse an expression in angle brackets: `<...>`.
#[inline(always)]
#[must_use]
pub fn angle_bracketed<Output>(p: Parser<u8, Output>) -> Parser<u8, Output> {
    wrapped(b'<', p, b'>')
}

#[cfg(feature = "nightly")]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn skip_while<Input, Predicate: Fn(&Input) -> bool>(
    pred: Predicate,
) -> Parser<Input, (), impl FnOnce(&[Input]) -> result::Result<((), &[Input])>> {
    Parser::new(move |mut slice| {
        while let Some((head, tail)) = slice.split_first() {
            if pred(head) {
                slice = tail;
            } else {
                break;
            }
        }
        Ok(((), slice))
    })
}

#[cfg(not(feature = "nightly"))]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn skip_while<Input, Predicate: 'static + Fn(&Input) -> bool>(
    pred: Predicate,
) -> Parser<Input, ()> {
    Parser::new(move |mut slice| {
        while let Some((head, tail)) = slice.split_first() {
            if pred(head) {
                slice = tail;
            } else {
                break;
            }
        }
        Ok(((), slice))
    })
}

parse_fn! {
    /// Zero or more whitespace characters.
    pub fn whitespace() -> (u8 => ()) {
        skip_while(|c| matches!(c, &b' ' | &b'\t' | &b'\r' | &b'\n'))
    }
}

#[cfg(feature = "nightly")]
/// Parse a comma-separated list (not in parentheses or anything—treat that separately).
#[inline(always)]
#[must_use]
pub fn comma_separated<
    Output,
    LazyCall: FnOnce(&[u8]) -> result::Result<(Output, &[u8])>,
    Lazy: 'static + Fn() -> Parser<u8, Output, LazyCall>,
>(
    p: Lazy,
) -> Parser<u8, Vec<Output>, impl FnOnce(&[u8]) -> result::Result<(Vec<Output>, &[u8])>> {
    Parser::new(move |slice| {
        let mut results = vec![];
        let mut long_term_etc = whitespace().0(slice)?.1;
        while let Ok((out, etc)) = (p() << whitespace()).0(long_term_etc) {
            results.push(out);
            long_term_etc = if let Ok((_, the_rest)) = (exact(b',') << whitespace()).0(etc) {
                the_rest
            } else {
                return Ok((results, etc));
            };
        }
        Ok((results, long_term_etc))
    })
}

#[cfg(not(feature = "nightly"))]
/// Parse a comma-separated list (not in parentheses or anything—treat that separately).
#[inline(always)]
#[must_use]
pub fn comma_separated<Output: 'static, Lazy: 'static + Fn() -> Parser<u8, Output>>(
    p: Lazy,
) -> Parser<u8, Vec<Output>> {
    Parser::new(move |slice| {
        let mut results = vec![];
        let mut long_term_etc = whitespace().0(slice)?.1;
        while let Ok((out, etc)) = (p() << whitespace()).0(long_term_etc) {
            results.push(out);
            long_term_etc = if let Ok((_, the_rest)) = (exact(b',') << whitespace()).0(etc) {
                the_rest
            } else {
                return Ok((results, etc));
            };
        }
        Ok((results, long_term_etc))
    })
}
