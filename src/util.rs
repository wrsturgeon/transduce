/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Common parsing utilities like parenthesized statements.

use crate::prelude::*;

parse_fn! {
    /// Match an exact value (via `PartialEq`) and discard it.
    pub fn exact<I: PartialEq>(expected: I) -> I => ()
    |i| if i != expected { bail!("`exact` failed: expected `{expected:#?}` but found `{i:#?}`") }
}

parse_fn! {
    /// Match any single item and return it.
    pub fn anything<I>() -> I => I
    |i| i
}

/// Match an expression in parentheses.
#[inline(always)]
#[must_use]
pub fn parenthesized<O>(p: Parser<char, O>) -> Parser<char, O> {
    exact('(') >> p << exact(')')
}
