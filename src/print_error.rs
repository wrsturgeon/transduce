/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Trait for pretty-printing parse errors.

/// Pretty-print parse errors.
pub trait PrintError: Sized + ::core::fmt::Debug {
    /// Print a parsing error in its context.
    #[inline(always)]
    #[allow(unused_variables)]
    fn pretty_error(msg: String, buffer: &[Self], index: Option<usize>) -> String {
        msg
    }
}

/// Find the last newline in this slice.
#[inline(always)]
fn last_newline(buffer: &[u8]) -> Option<usize> {
    #![allow(clippy::indexing_slicing)]
    (0..buffer.len()).rev().find(|&i| buffer[i] == b'\n')
}

/// Find the first newline in this slice.
#[inline(always)]
fn next_newline(buffer: &[u8]) -> Option<usize> {
    #![allow(clippy::indexing_slicing)]
    (0..buffer.len()).find(|&i| buffer[i] == b'\n')
}

impl PrintError for u8 {
    #[inline(always)]
    #[allow(clippy::too_many_lines)]
    fn pretty_error(msg: String, buffer: &[Self], maybe_index: Option<usize>) -> String {
        #![allow(clippy::indexing_slicing, clippy::string_add)]

        let len = buffer.len();
        let out_of_bounds = maybe_index.is_none();
        let index = maybe_index.unwrap_or(len);
        let nline = {
            let mut acc: usize = 1; // 1-indexed
            for c in &buffer[..index] {
                if c == &b'\n' {
                    acc = acc.saturating_add(1);
                }
            }
            acc
        };
        let ndigit = usize::max(
            2,
            match usize::try_from(nline.checked_ilog10().unwrap_or(0)) {
                Err(err) => {
                    return "Number of digits did not fit in a `usize`: ".to_owned()
                        + err.to_string().as_str()
                }
                Ok(ok) => ok,
            }
            .saturating_add(1),
        );

        let mut out = "\r\n".to_owned();

        // Header line:
        for _ in 0..ndigit.saturating_sub(2) {
            out.push(' ');
        }
        // 31: red
        // 32: green
        // 33: yellow
        // 34: blue
        // 35: purple
        // 36: light blue
        out.push_str("\x1B[0m...| \x1b[0;1;31mError while parsing:\x1b[0m\r\n");

        let line_end = next_newline(&buffer[index..]).map(|x| index.saturating_add(x));
        let line_begin = last_newline(&buffer[..index]);
        let begin = line_begin.map_or(0, |begin| {
            out.push_str(
                nline
                    .checked_sub(1)
                    .map_or_else(
                        || format!("{:>ndigit$} | ", '?'),
                        |line| format!("{line:>ndigit$} | "),
                    )
                    .as_str(),
            );
            out.push_str(
                core::str::from_utf8(
                    &buffer
                        [last_newline(&buffer[..begin]).map_or(0, |x| x.saturating_add(1))..begin],
                )
                .unwrap_or("[Input to parse was not valid UTF-8 and thus can't be displayed]"),
            );
            out.push_str("\r\n");
            begin
        });
        out.push_str(format!("{nline:>ndigit$} | ").as_str());
        out.push_str(
            core::str::from_utf8(&buffer[begin.saturating_add(1)..index])
                .unwrap_or("[Input to parse was not valid UTF-8 and thus can't be displayed]"),
        );
        out.push_str("\x1B[0;1;41m");
        if out_of_bounds {
            out.push(' ');
            out.push_str("\x1B[0m");
        } else {
            match buffer[index] {
                32.. => out.push(buffer[index].into()), // visible characters
                b'\n' => out.push_str("[newline]"),
                b'\r' => out.push_str("[carriage return, probably a newline]"),
                b'\t' => out.push_str("[tab]"),
                hex => out.push_str(format!("[raw ASCII character #{hex:}]").as_str()),
            }
            out.push_str("\x1B[0m");
            let next = index.saturating_add(1);
            let valid_end = line_end.unwrap_or(len);
            if next < valid_end {
                out.push_str(
                    core::str::from_utf8(&buffer[next..valid_end]).unwrap_or(
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
        for _ in line_begin.map_or(0, |x| x.saturating_add(1))..index {
            out.push(' ');
        }
        out.push_str("\x1B[0;1;31m^ ");
        out.push_str(msg.as_str());
        out.push_str("\x1B[0m\r\n");

        // Next line, if any
        if let Some(end) = line_end {
            out.push_str(
                nline
                    .checked_add(1)
                    .map_or_else(
                        || format!("{:>ndigit$} | ", '?'),
                        |line| format!("{line:>ndigit$} | "),
                    )
                    .as_str(),
            );
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
}
