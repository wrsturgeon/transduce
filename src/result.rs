/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Result type returning an error with a backtrace.

/// Parsing error: both a message and where it was, via accepting the length of input _after_ the error.
#[allow(clippy::exhaustive_structs)]
#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    /// Message helpfully describing this error.
    pub message: String,
    /// Length of input _after_ this error, or `None` if we ran out of input (would be -1).
    pub etc: Option<usize>,
}

/// Result: either an `A` or a `String`.
pub type Result<Output> = ::core::result::Result<Output, ParseError>;

/// Immediately return with an error. The second argument requests the slice of input _after_ this error, so we can deduce where it happened.
#[macro_export]
macro_rules! bail {
    ($fmsg:expr, $etc:expr) => {
        return ::core::result::Result::Err(ParseError {
            message: ::std::format!($fmsg),
            etc: Some(($etc).len()),
        })
    };
}
