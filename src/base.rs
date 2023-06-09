/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Common functions to drop in instead of reinventing the wheel.

#![allow(unreachable_code)] // TODO: REMOVE

use crate::{bail, result::ParseError, Parser};
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

/// End-of-input error: expected an item but none remaining.
macro_rules! end_of_input {
    () => {
        return ::core::result::Result::Err(ParseError {
            message: "Reached end of input but expected an item".to_owned(),
            etc: None,
        })
    };
}

#[cfg(feature = "nightly")]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn satisfies<Input: Clone, Predicate: Fn(&Input) -> bool>(
    pred: Predicate,
) -> Parser<Input, Input, impl FnOnce(&[Input]) -> result::Result<(Input, &[Input])>> {
    Parser::new(move |slice| {
        if let Some((head, tail)) = slice.split_first() {
            if pred(head) {
                Ok((head.clone(), tail))
            } else {
                bail!("Predicate not satisfied", tail)
            }
        } else {
            end_of_input!()
        }
    })
}

#[cfg(not(feature = "nightly"))]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn satisfies<Input: Clone, Predicate: 'static + Fn(&Input) -> bool>(
    pred: Predicate,
) -> Parser<Input, Input> {
    Parser::new(move |slice| {
        if let Some((head, tail)) = slice.split_first() {
            if pred(head) {
                Ok((head.clone(), tail))
            } else {
                bail!("Predicate not satisfied", tail)
            }
        } else {
            end_of_input!()
        }
    })
}

parse_fn! {
    /// Match the end of a stream.
    pub fn end<I>() -> (I => ()) {
        Parser::new(move |slice| {
            if !slice.is_empty() {
                bail!("Tried to match the end of a stream but found input remaining", slice)
            }
            Ok(((), &[]))
        })
    }
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and return a clone.
    pub fn exact<I: 'static + Clone + PartialEq + Debug>(expect: I) -> (I => I) {
        Parser::new(move |slice: &[I]| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head == &expect {
                    Ok((head.clone(), tail))
                } else {
                    bail!("Expected {expect:#?} but found {head:#?}", tail)
                }
            }
        })
    }
}

parse_fn! {
    /// Match any single item and return it.
    pub fn verbatim<I: Clone>() -> (I => I) {
        Parser::new(move |slice: &[I]| {
            if let Some((head, tail)) = slice.split_first() {
                Ok((head.clone(), tail))
            } else {
                end_of_input!()
            }
        })
    }
}

parse_fn! {
    /// Match a lowercase letter and return it.
    pub fn lowercase() -> (u8 => u8) {
        Parser::new(move |slice: &[u8]| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head.is_ascii_lowercase() {
                    Ok((*head, tail))
                } else {
                    let c: char = (*head).into();
                    bail!("Expected a lowercase letter but found '{c:#?}'", tail)
                }
            }
        })
    }
}

parse_fn! {
    /// Match an uppercase letter and return it.
    pub fn uppercase() -> (u8 => u8) {
        Parser::new(move |slice: &[u8]| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head.is_ascii_uppercase() {
                    Ok((*head, tail))
                } else {
                    let c: char = (*head).into();
                    bail!("Expected an uppercase letter but found '{c:#?}'", tail)
                }
            }
        })
    }
}

parse_fn! {
    /// Match a single digit and return it (as an integer, not a character).
    pub fn digit() -> (u8 => u8) {
        Parser::new(move |slice: &[u8]| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head.is_ascii_digit() {
                    Ok((head.checked_sub(b'0').ok_or_else(|| ParseError { message: "Internal error: `digit` received an out-of-range character".to_owned(), etc: Some(tail.len()) })?, tail))
                } else {
                    let c: char = (*head).into();
                    bail!("Expected a digit but found '{c:#?}'", tail)
                }
            }
        })
    }
}

#[cfg(feature = "nightly")]
/// If you can match, return it; if not, stay in the same place.
#[inline(always)]
#[must_use]
pub fn optional<Output, Call: FnOnce(&[u8]) -> result::Result<(Output, &[u8])>>(
    p: Parser<u8, Output, Call>,
) -> Parser<u8, Option<Output>, impl FnOnce(&[u8]) -> result::Result<(Option<Output>, &[u8])>> {
    Parser::new(move |stream| match p.0(stream) {
        Ok((parsed, etc)) => Ok((Some(parsed), etc)),
        Err(_) => Ok((None, stream)),
    })
}

#[cfg(not(feature = "nightly"))]
/// If you can match, return it; if not, stay in the same place.
#[inline(always)]
#[must_use]
pub fn optional<Output>(p: Parser<u8, Output>) -> Parser<u8, Option<Output>> {
    Parser::new(move |stream| match p.0(stream) {
        Ok((parsed, etc)) => Ok((Some(parsed), etc)),
        Err(_) => Ok((None, stream)),
    })
}

#[cfg(feature = "nightly")]
/// Parse an expression between two (discarded) items.
#[inline(always)]
#[must_use]
pub fn wrapped<
    Input: PartialEq + Clone + Debug,
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
pub fn wrapped<Input: 'static + PartialEq + Clone + Debug, Output>(
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
