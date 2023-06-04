/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Result type returning an error with a backtrace.

/// Result: either an `A` or a `String`.
pub type Result<A = ()> = ::core::result::Result<A, String>;

/// Immediately return with an error.
#[macro_export]
macro_rules! bail {
    ($tt:tt) => {
        return ::core::result::Result::Err(::std::format!($tt))
    };
}
