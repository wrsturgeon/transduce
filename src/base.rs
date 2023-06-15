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
pub fn satisfies<'input, 'parser, Input, Predicate: 'parser + FnOnce(&'input Input) -> bool>(
    pred: Predicate,
) -> Parser<
    'input,
    'parser,
    Input,
    &'input Input,
    impl 'parser + FnOnce(&'input [Input]) -> result::Result<(&'input Input, &'input [Input])>,
> {
    Parser::new(|slice| {
        if let Some((head, tail)) = slice.split_first() {
            if pred(head) {
                Ok((head, tail))
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
pub fn satisfies<'input, 'parser, Input, Predicate: 'parser + FnOnce(&'input Input) -> bool>(
    pred: Predicate,
) -> Parser<'input, 'parser, Input, &'input Input> {
    Parser::new(|slice| {
        if let Some((head, tail)) = slice.split_first() {
            if pred(head) {
                Ok((head, tail))
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
        Parser::new(|slice| {
            if !slice.is_empty() {
                bail!("Tried to match the end of a stream but found input remaining", slice)
            }
            Ok(((), &[]))
        })
    }
}

parse_fn! {
    /// Match an exact value (via `PartialEq`) and return a clone.
    pub fn exact<I: PartialEq + Debug>(expect: &'parser I) -> (I => &'input I) {
        Parser::new(move |slice| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head == expect {
                    Ok((head, tail))
                } else {
                    bail!("Expected {expect:#?} but found {head:#?}", tail)
                }
            }
        })
    }
}

parse_fn! {
    /// Match an exact sequence of items (via `PartialEq`) and return a reference to the original, not the parsed input.
    pub fn exact_seq<I: PartialEq + Debug>(expect: &'parser [I]) -> (I => &'parser [I]) {
        Parser::new(move |slice| {
            let mut etc = slice;
            for i in expect {
                match etc.split_first() {
                    None => end_of_input!(),
                    Some((head, tail)) => {
                        if head != i {
                            bail!(
                                "Expected {expect:#?} but found {head:#?} instead of {i:#?}",
                                tail
                            )
                        }
                        etc = tail;
                    }
                }
            }
            Ok((expect, etc))
        })
    }
}

parse_fn! {
    /// Match any of a set of options. Set represented as a binary tree for efficiency.
    pub fn any<I: Debug + Ord>(expect: &'parser ::alloc::collections::BTreeSet<I>) -> (I => &'parser I) {
        Parser::new(move |slice| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if let Some(in_set) = expect.get(head) {
                    Ok((in_set, tail))
                } else {
                    bail!(
                        "Expected an element of {expect:#?} but found {head:#?}",
                        tail
                    )
                }
            }
        })
    }
}

parse_fn! {
    /// Match any of a set of options. Set represented as a binary tree for efficiency.
    pub fn any_seq<I: Debug + Ord>(expect: &'parser ::alloc::collections::BTreeSet<&'input [I]>) -> (I => &'input [I]) {
        Parser::new(move |slice| {
            let Some((head, _)) = slice.split_first() else { end_of_input!() };
            for option in expect.range(core::slice::from_ref(head)..=slice) {
                if let ok @ Ok(_) = exact_seq(option).once(slice) {
                    return ok;
                }
            }
            match slice.split_first() {
                Some((_, tail)) => bail!("Expected one of {expect:#?}", tail),
                None => end_of_input!(),
            }
        })
    }
}

parse_fn! {
    /// Match any single item and return it.
    pub fn verbatim<I>() -> (I => &'input I) {
        Parser::new(|slice: &[I]| {
            if let Some((head, tail)) = slice.split_first() {
                Ok((head, tail))
            } else {
                end_of_input!()
            }
        })
    }
}

parse_fn! {
    /// Match a lowercase letter and return it.
    pub fn lowercase() -> (u8 => &'input u8) {
        Parser::new(|slice: &[u8]| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head.is_ascii_lowercase() {
                    Ok((head, tail))
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
    pub fn uppercase() -> (u8 => &'input u8) {
        Parser::new(|slice: &[u8]| match slice.split_first() {
            None => end_of_input!(),
            Some((head, tail)) => {
                if head.is_ascii_uppercase() {
                    Ok((head, tail))
                } else {
                    let c: char = (*head).into();
                    bail!("Expected an uppercase letter but found '{c:#?}'", tail)
                }
            }
        })
    }
}

parse_fn! {
    /// Match a `snake_case` term.
    pub fn snake_case() -> (u8 => Vec<u8>) {
        Parser::new(|slice| {
            let mut r = vec![];
            let mut etc = slice;
            loop {
                if let Ok((head, tail)) = (lowercase() | || exact(&b'_')).once(etc) {
                    r.push(*head);
                    etc = tail;
                } else if whitespace().once(etc).is_ok() {
                    return Ok((r, etc))
                } else {
                    bail!("Expected `snake_case`", etc)
                }
            }
        })
    }
}

parse_fn! {
    /// Match a `SCREAMING_CASE` term.
    pub fn screaming_case() -> (u8 => Vec<u8>) {
        Parser::new(|slice| {
            let mut r = vec![];
            let mut etc = slice;
            loop {
                if let Ok((head, tail)) = (uppercase() | || exact(&b'_')).once(etc) {
                    r.push(*head);
                    etc = tail;
                } else if whitespace().once(etc).is_ok() {
                    return Ok((r, etc))
                } else {
                    bail!("Expected `SCREAMING_CASE`", etc)
                }
            }
        })
    }
}

parse_fn! {
    /// Match a `lowerCamelCase` term.
    pub fn lower_camel_case() -> (u8 => Vec<u8>) {
        Parser::new(|slice| {
            let Some((first, mut etc)) = slice.split_first() else { end_of_input!() };
            if lowercase().once(slice).is_err() {
                bail!("Expected `lowerCamelCase`", etc);
            }
            let mut r = vec![*first];
            loop {
                if let Ok((head, tail)) = (lowercase() | uppercase).once(etc) {
                    r.push(*head);
                    etc = tail;
                } else if whitespace().once(etc).is_ok() {
                    return Ok((r, etc))
                } else {
                    bail!("Expected `lowerCamelCase`", etc)
                }
            }
        })
    }
}

parse_fn! {
    /// Match an `UpperCamelCase` term.
    pub fn upper_camel_case() -> (u8 => Vec<u8>) {
        Parser::new(|slice| {
            let Some((first, mut etc)) = slice.split_first() else { end_of_input!() };
            if uppercase().once(slice).is_err() {
                bail!("Expected `UpperCamelCase`", etc);
            }
            let mut r = vec![*first];
            loop {
                if let Ok((head, tail)) = (lowercase() | uppercase).once(etc) {
                    r.push(*head);
                    etc = tail;
                } else if whitespace().once(etc).is_ok() {
                    return Ok((r, etc))
                } else {
                    bail!("Expected `lowerCamelCase`", etc)
                }
            }
        })
    }
}

parse_fn! {
    /// Match a single digit and return it (as an integer, not a character).
    pub fn digit() -> (u8 => u8) {
        Parser::new(|slice: &[u8]| match slice.split_first() {
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

parse_fn! {
    /// Match a base-ten unsigned integer.
    pub fn unsigned_integer() -> (u8 => usize) {
        Parser::new(|slice| {
            let Some((_, mut etc)) = slice.split_first() else { end_of_input!() };
            let mut r = match digit().once(slice) {
                Ok((i, _)) => usize::from(i),
                Err(_) => bail!("Expected `UpperCamelCase`", etc),
            };
            loop {
                if let Ok((head, tail)) = digit().once(etc) {
                    r = match r.checked_mul(10).map(|x| x.checked_add(usize::from(head))) {
                        Some(Some(x)) => x,
                        _ => bail!("Overflow: value is too big to fit into a Rust `usize`", tail),
                    };
                    etc = tail;
                } else if whitespace().once(etc).is_ok() {
                    return Ok((r, etc))
                } else {
                    bail!("Expected `lowerCamelCase`", etc)
                }
            }
        })
    }
}

parse_fn! {
    /// Match a base-ten signed integer.
    pub fn signed_integer() -> (u8 => isize) {
        Parser::new(|slice| {
            let ((neg, abs), etc) = (optional(|| exact(&b'-')) & unsigned_integer()).once(slice)?;
            let Ok(val) = isize::try_from(abs) else {
                bail!("Overflow: value is too big to fit into a Rust `isize`", etc);
            };
            let signed = if neg.is_some() {
                match val.checked_neg() {
                    Some(ok) => ok,
                    None => bail!("Used this integer's most negative value, whose positive can't be represented", etc),
                }
            } else {
                val
            };
            Ok((signed, etc))
        })
    }
}

#[cfg(feature = "nightly")]
/// If you can match, return it; if not, stay in the same place.
#[inline(always)]
#[must_use]
pub fn optional<
    'input,
    'parser,
    Output,
    Call: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output, Call>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    u8,
    Option<Output>,
    impl FnOnce(&'input [u8]) -> result::Result<(Option<Output>, &'input [u8])>,
> {
    Parser::new(|stream| match p().once(stream) {
        Ok((parsed, etc)) => Ok((Some(parsed), etc)),
        Err(_) => Ok((None, stream)),
    })
}

#[cfg(not(feature = "nightly"))]
/// If you can match, return it; if not, stay in the same place.
#[inline(always)]
#[must_use]
pub fn optional<
    'input,
    'parser,
    Output: 'parser,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, u8, Option<Output>> {
    Parser::new(|stream| match p().once(stream) {
        Ok((parsed, etc)) => Ok((Some(parsed), etc)),
        Err(_) => Ok((None, stream)),
    })
}

// FIXME: investigate https://github.com/rust-lang/rust/blob/master/tests/ui/generic-associated-types/issue-88595.stderr
/*
#[cfg(feature = "nightly")]
/// Parse an expression between two (discarded) items.
#[inline(always)]
#[must_use]
pub fn wrapped<
    'input,
    'parser,
    Input: PartialEq + Debug,
    Output: 'parser,
    Call: 'parser + FnOnce(&'input [Input]) -> result::Result<(Output, &'input [Input])>,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, Input, Output, Call>,
>(
    before: &'parser Input,
    p: Lazy,
    after: &'parser Input,
) -> Parser<
    'input,
    'parser,
    Input,
    Output,
    impl 'parser + FnOnce(&'input [Input]) -> result::Result<(Output, &'input [Input])>,
> {
    exact(before) >> p() << exact(after)
}
*/

#[cfg(not(feature = "nightly"))]
/// Parse an expression between two (discarded) items.
#[inline(always)]
#[must_use]
pub fn wrapped<
    'input,
    'parser,
    Input: PartialEq + Debug,
    Output: 'parser,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, Input, Output>,
>(
    before: &'parser Input,
    p: Lazy,
    after: &'parser Input,
) -> Parser<'input, 'parser, Input, Output> {
    exact(before) >> p() << exact(after)
}

// FIXME
/*
#[cfg(feature = "nightly")]
/// Parse an expression in parentheses: `(...)`.
#[inline(always)]
#[must_use]
pub fn parenthesized<
    'input,
    'parser,
    Output,
    Call: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output, Call>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    u8,
    Output,
    impl 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
> {
    wrapped(&b'(', p, &b')')
}
*/

#[cfg(not(feature = "nightly"))]
/// Parse an expression in parentheses: `(...)`.
#[inline(always)]
#[must_use]
pub fn parenthesized<
    'input,
    'parser,
    Output: 'parser,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, u8, Output> {
    wrapped(&b'(', p, &b')')
}

// FIXME
/*
#[cfg(feature = "nightly")]
/// Parse an expression in brackets: `[...]`.
#[inline(always)]
#[must_use]
pub fn bracketed<
    'input,
    'parser,
    Output,
    Call: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output, Call>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    u8,
    Output,
    impl 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
> {
    wrapped(&b'[', p, &b']')
}
*/

#[cfg(not(feature = "nightly"))]
/// Parse an expression in brackets: `[...]`.
#[inline(always)]
#[must_use]
pub fn bracketed<
    'input,
    'parser,
    Output: 'parser,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, u8, Output> {
    wrapped(&b'[', p, &b']')
}

// FIXME
/*
#[cfg(feature = "nightly")]
/// Parse an expression in curly braces: `{...}`.
#[inline(always)]
#[must_use]
pub fn braced<
    'input,
    'parser,
    Output,
    Call: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output, Call>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    u8,
    Output,
    impl 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
> {
    wrapped(&b'{', p, &b'}')
}
*/

#[cfg(not(feature = "nightly"))]
/// Parse an expression in curly braces: `{...}`.
#[inline(always)]
#[must_use]
pub fn braced<
    'input,
    'parser,
    Output: 'parser,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, u8, Output> {
    wrapped(&b'{', p, &b'}')
}

// FIXME
/*
#[cfg(feature = "nightly")]
/// Parse an expression in angle brackets: `<...>`.
#[inline(always)]
#[must_use]
pub fn angle_bracketed<
    'input,
    'parser,
    Output,
    Stream,
    Call: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output, Call>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    u8,
    Output,
    impl 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
> {
    wrapped(&b'<', p, &b'>')
}
*/

#[cfg(not(feature = "nightly"))]
/// Parse an expression in angle brackets: `<...>`.
#[inline(always)]
#[must_use]
pub fn angle_bracketed<
    'input,
    'parser,
    Output: 'parser,
    Lazy: 'parser + FnOnce() -> Parser<'input, 'parser, u8, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, u8, Output> {
    wrapped(&b'<', p, &b'>')
}

#[cfg(feature = "nightly")]
/// Skip zero or more items while this predicate holds on them. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn skip_while<'input, 'parser, Input, Predicate: 'parser + Fn(&'input Input) -> bool>(
    pred: Predicate,
) -> Parser<
    'input,
    'parser,
    Input,
    (),
    impl 'parser + FnOnce(&'input [Input]) -> result::Result<((), &'input [Input])>,
> {
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
pub fn skip_while<'input, 'parser, Input, Predicate: 'parser + Fn(&'input Input) -> bool>(
    pred: Predicate,
) -> Parser<'input, 'parser, Input, ()> {
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

#[cfg(feature = "nightly")]
/// Parse as many items as possible while the parser doesn't return an error. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn parse_while<
    'input,
    'parser,
    Input,
    Output,
    Call: 'parser + FnOnce(&'input [Input]) -> result::Result<(Output, &'input [Input])>,
    Lazy: 'parser + Fn() -> Parser<'input, 'parser, Input, Output, Call>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    Input,
    Vec<Output>,
    impl 'parser + FnOnce(&'input [Input]) -> result::Result<(Vec<Output>, &'input [Input])>,
> {
    Parser::new(move |slice| {
        let mut v = vec![];
        let mut etc = slice;
        while let Ok((parsed, next_etc)) = p().once(etc) {
            v.push(parsed);
            etc = next_etc;
        }
        Ok((v, slice))
    })
}

#[cfg(not(feature = "nightly"))]
/// Parse as many items as possible while the parser doesn't return an error. Do not skip the first one that doesn't hold (just peek, don't consume prematurely).
#[inline(always)]
#[must_use]
pub fn parse_while<
    'input,
    'parser,
    Input,
    Output: 'parser,
    Lazy: 'parser + Fn() -> Parser<'input, 'parser, Input, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, Input, Vec<Output>> {
    Parser::new(move |slice| {
        let mut v = vec![];
        let mut etc = slice;
        while let Ok((parsed, next_etc)) = p().once(etc) {
            v.push(parsed);
            etc = next_etc;
        }
        Ok((v, slice))
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
    'input,
    'parser,
    Output,
    LazyCall: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
    Lazy: 'parser + Fn() -> Parser<'input, 'parser, u8, Output, LazyCall>,
>(
    p: Lazy,
) -> Parser<
    'input,
    'parser,
    u8,
    Vec<Output>,
    impl 'parser + FnOnce(&'input [u8]) -> result::Result<(Vec<Output>, &'input [u8])>,
> {
    Parser::new(move |slice| {
        let mut results = vec![];
        let mut long_term_etc = whitespace().once(slice)?.1;
        while let Ok((out, etc)) = (p() << whitespace()).once(long_term_etc) {
            results.push(out);
            long_term_etc = if let Ok((_, the_rest)) = (exact(&b',') << whitespace()).once(etc) {
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
pub fn comma_separated<
    'input,
    'parser,
    Output: 'parser,
    Lazy: 'parser + Fn() -> Parser<'input, 'parser, u8, Output>,
>(
    p: Lazy,
) -> Parser<'input, 'parser, u8, Vec<Output>> {
    Parser::new(move |slice| {
        let mut results = vec![];
        let mut long_term_etc = whitespace().once(slice)?.1;
        while let Ok((out, etc)) = (p() << whitespace()).once(long_term_etc) {
            results.push(out);
            long_term_etc = if let Ok((_, the_rest)) = (exact(&b',') << whitespace()).once(etc) {
                the_rest
            } else {
                return Ok((results, etc));
            };
        }
        Ok((results, long_term_etc))
    })
}

/// Operator-precedence parsing, e.g. for infix math.
pub mod precedence {
    #[allow(clippy::wildcard_imports)]
    use super::*;

    #[cfg(feature = "nightly")]
    /// Parse binary operations without grouping them by precedence into a tree: just return a list of operators and things to the right of them.
    #[inline(always)]
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn raw_binops<
        'input,
        'parser,
        Output,
        Call: 'parser + FnOnce(&'input [u8]) -> result::Result<(Output, &'input [u8])>,
        Lazy: 'parser + Fn() -> Parser<'input, 'parser, u8, Output, Call>,
    >(
        primary: Lazy,
        operators: &'parser ::alloc::collections::BTreeSet<&'input [u8]>,
    ) -> Parser<
        'input,
        'parser,
        u8,
        (Output, Vec<(&'input [u8], Output)>),
        impl 'parser
            + FnOnce(
                &'input [u8],
            )
                -> result::Result<((Output, Vec<(&'input [u8], Output)>), &'input [u8])>,
    > {
        Parser::new(move |slice| {
            let (first, first_etc) = primary().once(slice)?;
            let (rest, etc) =
                parse_while(&|| whitespace() >> any_seq(operators) & whitespace() >> primary())
                    .once(first_etc)?;
            Ok(((first, rest), etc))
        })
    }

    #[cfg(not(feature = "nightly"))]
    /// Parse binary operations without grouping them by precedence into a tree: just return a list of operators and things to the right of them.
    #[inline(always)]
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn raw_binops<
        'input,
        'parser,
        Output: 'parser,
        Lazy: 'parser + Fn() -> Parser<'input, 'parser, u8, Output>,
    >(
        primary: Lazy,
        operators: &'parser ::alloc::collections::BTreeSet<&'input [u8]>,
    ) -> Parser<'input, 'parser, u8, (Output, Vec<(&'input [u8], Output)>)> {
        Parser::new(move |slice| {
            let (first, first_etc) = primary().once(slice)?;
            let (rest, etc) =
                parse_while(|| whitespace() >> any_seq(operators) & whitespace() >> primary())
                    .once(first_etc)?;
            Ok(((first, rest), etc))
        })
    }
}
