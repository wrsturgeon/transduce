/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Trait for pretty-printing parse errors.

use crate::Message;

#[cfg(feature = "alloc")]
use crate::string_if_alloc;

#[cfg(feature = "alloc")]
use alloc::format;

/// Pretty-print parse errors.
pub trait PrintError: Sized + ::core::fmt::Debug {
    /// Print a parsing error in its context.
    #[inline(always)]
    #[must_use]
    #[allow(unused_variables)]
    fn pretty_error(msg: Message, buffer: &[Self], index: Option<usize>) -> Message {
        msg
    }
}

#[cfg(feature = "alloc")]
/// Find the last newline in this slice.
#[inline(always)]
fn last_newline(buffer: &[u8]) -> Option<usize> {
    #![allow(clippy::indexing_slicing)]
    (0..buffer.len()).rev().find(|&i| buffer[i] == b'\n')
}

#[cfg(feature = "alloc")]
/// Find the first newline in this slice.
#[inline(always)]
fn next_newline(buffer: &[u8]) -> Option<usize> {
    #![allow(clippy::indexing_slicing)]
    (0..buffer.len()).find(|&i| buffer[i] == b'\n')
}

impl PrintError for u8 {
    #[cfg(feature = "alloc")]
    #[inline(always)]
    #[allow(clippy::too_many_lines)]
    fn pretty_error(msg: Message, buffer: &[Self], maybe_index: Option<usize>) -> Message {
        #![allow(clippy::indexing_slicing, clippy::string_add)]

        use core::fmt::Write;

        let len = buffer.len();
        let out_of_bounds = maybe_index.is_none();
        let index = maybe_index.unwrap_or(len);
        let nline = {
            let mut acc: usize = 1; // 1-indexed
            for c in &buffer[..index] {
                if c == &b'\n' {
                    match acc.checked_add(1) {
                        Some(x) => acc = x,
                        None => {
                            return string_if_alloc(
                                "Parsing error: too many lines of input (would overflow a Rust `usize`)"
                            );
                        }
                    };
                }
            }
            acc
        };
        let ndigit = match usize::try_from(nline.checked_ilog10().unwrap_or(0)) {
            Err(_) => return string_if_alloc("Parsing error: Digits needed to represent the number of lines of input would overflow a Rust `usize`"),
            Ok(ok) => match ok.checked_add(2) {
                Some(x) => x,
                None => return string_if_alloc("Parsing error: Digits needed to represent the number of lines of input would overflow a Rust `usize`"),
            }
        };

        let mut out = string_if_alloc("\r\n");

        // Header line:
        for _ in 0..ndigit {
            out.push(' ');
        }
        out.push_str(" | \x1b[0;1;31mError while parsing:\x1b[0m\r\n");

        let line_end =
            match next_newline(&buffer[index..]) {
                Some(x) => match x.checked_add(index) {
                    s @ Some(_) => s,
                    None => return string_if_alloc(
                        "Parsing error: index of the next newline would overflow a Rust `usize`",
                    ),
                },
                None => None,
            };
        let line_begin = last_newline(&buffer[..index]).map_or_else(
            || 0, // No previous line
            |nl| {
                // Print the previous line
                out.push_str(&nline.checked_sub(1).map_or_else(
                    || format!("{:>ndigit$} | ", '?'),
                    |line| format!("{line:>ndigit$} | "),
                ));
                out.push_str(
                    core::str::from_utf8(
                        &buffer[last_newline(&buffer[..nl]).map_or(0, |x| x.saturating_add(1))..nl],
                    )
                    .unwrap_or("[Input to parse was not valid UTF-8 and thus can't be displayed]"),
                );
                out.push_str("\r\n");
                nl.saturating_add(1) // start of the current line
            },
        );
        if write!(out, "{nline:>ndigit$} | ").is_err() {
            return string_if_alloc(
                "Internal parsing error: Couldn't write to a Rust heap-allocated string",
            );
        }
        out.push_str(
            core::str::from_utf8(&buffer[line_begin..index])
                .unwrap_or("[Input to parse was not valid UTF-8 and thus can't be displayed]"),
        );
        out.push_str("\x1B[0;1;41m");
        if out_of_bounds {
            out.push(' ');
            out.push_str("\x1B[0m");
        } else {
            if let Some(c) = buffer.get(index) {
                match c {
                    &(32..) => out.push(buffer[index].into()), // visible characters
                    &(b'\n' | b'\r' | b'\t') => out.push(' '),
                    &hex => {
                        if write!(
                            out,
                            "[raw ASCII character #{hex:} = '{:}']",
                            char::from(hex)
                        )
                        .is_err()
                        {
                            return string_if_alloc(
                                "Internal parsing error: Couldn't write to a Rust heap-allocated string",
                            );
                        }
                    }
                }
            } else {
                return string_if_alloc(
                    "Parsing error: Given an index >= the entire length of input",
                );
            }
            out.push_str("\x1B[0m");
            let i0 = index.saturating_add(1);
            let i1 = line_end.unwrap_or(len);
            if i0 < i1 {
                out.push_str(
                    core::str::from_utf8(&buffer[i0..i1]).unwrap_or(
                        "[Input to parse was not valid UTF-8 and thus can't be displayed]",
                    ),
                );
            }
        }
        out.push_str("\r\n");

        // Error message itself:
        for _ in 0..ndigit {
            out.push(' ');
        }
        out.push_str(" | ");
        for _ in line_begin..index {
            out.push(' ');
        }
        out.push_str("\x1B[0;1;31m^ ");
        out.push_str(msg.as_str());
        out.push_str("\x1B[0m\r\n");

        // Next line, if any
        if let Some(end) = line_end {
            out.push_str(&nline.checked_add(1).map_or_else(
                || format!("{:>ndigit$} | ", '?'),
                |line| format!("{line:>ndigit$} | "),
            ));
            let next_end = next_newline(&buffer[end.saturating_add(1)..])
                .map_or(len, |x| x.saturating_add(end.saturating_add(1)));
            out.push_str(
                core::str::from_utf8(&buffer[end.saturating_add(1)..next_end])
                    .unwrap_or("[Input to parse was not valid UTF-8 and thus can't be displayed]"),
            );
            out.push_str("\r\n");
        }

        out
    }

    #[cfg(not(feature = "alloc"))]
    #[inline(always)]
    fn pretty_error(msg: Message, _: &[Self], _: Option<usize>) -> Message {
        msg
    }
}
