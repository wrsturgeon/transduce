/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Common functions to drop in instead of reinventing the wheel.

#![allow(clippy::tests_outside_test_module)]

use crate::{print_error::PrintError, *};
use alloc::{collections::BTreeSet, format, string::String, vec /* the macro */, vec::Vec};
use core::marker::PhantomData;

/// Property test active only when testing (not in debug or release builds).
macro_rules! ptest {
    ($($tt:tt)*) => {
        #[cfg(test)]
        ::proptest::proptest! { $($tt)* }
    };
}

/// End-of-input error: expected an item but none remaining.
macro_rules! end_of_input {
    () => {
        return Err(ParseError {
            message: String::from("Reached end of input but expected an item"),
            not_yet_parsed: None,
        })
    };
}

/// Make sure we've reached exactly the end of the stream of input; don't advance to it.
#[derive(Debug)]
pub struct End<Input: PrintError>(PhantomData<Input>);
impl<'input, Input: 'input + PrintError> Parse<'input> for End<Input> {
    type Input = Input;
    type Output = ();
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        if !input.is_empty() {
            bail!(
                "Expected the end of the input stream but found input remaining",
                input
            )
        }
        Ok(((), &[]))
    }
}
/// Make sure we've reached exactly the end of the stream of input; don't advance to it.
#[inline(always)]
#[must_use]
pub const fn end<'input, Input: 'input + PrintError>() -> Parser<'input, End<Input>> {
    Parser::new(End(PhantomData))
}
ptest! {
    #[test]
    fn prop_end(slice: Vec<u8>) {
        assert_eq!(end().parse(&slice).is_ok(), slice.is_empty());
    }
}

/// Match any single item and return a reference to it.
#[derive(Debug)]
pub struct Anything<Input: PrintError>(PhantomData<Input>);
impl<'input, Input: 'input + PrintError> Parse<'input> for Anything<Input> {
    type Input = Input;
    type Output = &'input Input;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        if let Some((head, tail)) = input.split_first() {
            Ok((head, tail))
        } else {
            end_of_input!()
        }
    }
}
/// Match any single item and return a reference to it.
#[inline(always)]
#[must_use]
pub const fn anything<'input, Input: 'input + PrintError>() -> Parser<'input, Anything<Input>> {
    Parser::new(Anything(PhantomData))
}
ptest! {
    #[test]
    fn prop_anything(input: u8) {
        assert_eq!(anything().parse(&[input]), Ok(&input));
    }
}

// TODO: const (nightly?)
/// Skip zero or more items while this predicate holds on them.
#[inline(always)]
#[must_use]
pub fn satisfy<
    'input,
    Input: 'input + PrintError,
    Predicate: Fn(&'input Input) -> bool,
    WriteMessage: Fn(&'input Input) -> String,
>(
    predicate: Predicate,
    msg: WriteMessage,
) -> Parser<'input, impl Parse<'input, Input = Input, Output = &'input Input>> {
    anything() ^ move |x| if predicate(x) { Ok(x) } else { Err(msg(x)) }
}
ptest! {
    #[test]
    fn prop_satisfy(input: u8, threshold: u8) {
        let pred = move |x: &u8| x < &threshold;
        let slice = &[input];
        assert_eq!(pred(&input), satisfy(pred, |_| String::new()).parse(slice).is_ok());
    }
}

// TODO: const (nightly?)
/// Skip zero or more items while this predicate holds on them.
#[inline(always)]
#[must_use]
pub fn satisfy_result<
    'input,
    Input: 'input + PrintError,
    Predicate: Fn(&'input Input) -> Result<(), String>,
>(
    predicate: Predicate,
) -> Parser<'input, impl Parse<'input, Input = Input, Output = &'input Input>> {
    anything() ^ move |x| predicate(x).map(|()| x)
}
ptest! {
    #[test]
    fn prop_satisfy_result(input: u8, threshold: u8) {
        let pred = move |x: &u8| x < &threshold;
        let slice = &[input];
        assert_eq!(pred(&input), satisfy_result(move |x| if pred(x) { Ok(()) } else { Err(String::new()) }).parse(slice).is_ok());
    }
}

// TODO: const
/// Return anything that does _not_ satisfy this predicate.
#[inline(always)]
#[must_use]
pub fn not<
    'input,
    Input: 'input + PrintError,
    Predicate: Fn(&'input Input) -> bool,
    WriteMessage: Fn(&'input Input) -> String,
>(
    predicate: Predicate,
    msg: WriteMessage,
) -> Parser<'input, impl Parse<'input, Input = Input, Output = &'input Input>> {
    satisfy(move |x| !predicate(x), msg)
}
ptest! {
    #[test]
    fn prop_not(input: u8, threshold: u8) {
        let pred = move |x: &u8| x < &threshold;
        let slice = &[input];
        assert_eq!(pred(&input), not(pred, |_| String::new()).parse(slice).is_err());
    }
}

// TODO: const
/// Match exactly this item and return a reference to the original (not the parsed one).
#[inline(always)]
#[must_use]
pub fn exact<'input, Input: PartialEq<Input> + PrintError>(
    reference: &'input Input,
) -> Parser<'input, impl Parse<'input, Input = Input, Output = &'input Input>> {
    satisfy(
        move |x| reference.eq(x),
        move |head| format!("Expected {reference:#?} but found {head:#?}"),
    )
}
ptest! {
    #[test]
    fn prop_exact(a: u8, b: u8) {
        let slice_a = &[a];
        let slice_b = &[b];
        assert_eq!(exact(&a).parse(slice_a), Ok(&a));
        assert_eq!(exact(&a).parse(slice_b).is_ok(), a == b);
    }
}

/// Match exactly this sequence of items.
#[derive(Debug)]
pub struct ExactSeq<'platonic, Input: PartialEq + PrintError>(&'platonic [Input]);
impl<'input, Input: 'input + PartialEq + PrintError> Parse<'input> for ExactSeq<'_, Input> {
    type Input = Input;
    type Output = &'input [Input];
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let mut etc = input;
        for i in self.0 {
            match etc.split_first() {
                None => end_of_input!(),
                Some((head, tail)) => {
                    if head != i {
                        let e = self.0;
                        bail!(
                            "Expected {e:#?} but found {head:#?} instead of {i:#?}",
                            tail
                        )
                    }
                    etc = tail;
                }
            }
        }
        Ok((input.split_at(self.0.len()).0, etc))
    }
}
/// Match exactly this sequence of items.
#[inline(always)]
#[must_use]
pub const fn exact_seq<'platonic, 'input, Input: 'input + PartialEq + PrintError>(
    exactly: &'platonic [Input],
) -> Parser<'input, ExactSeq<'platonic, Input>> {
    Parser::new(ExactSeq(exactly))
}
ptest! {
    #[test]
    fn prop_exact_seq(a: Vec<u8>, b: Vec<u8>) {
        assert_eq!(exact_seq(&a).parse(&a), Ok(&*a));
        assert_eq!(exact_seq(&a).parse(&b).is_ok(), a == b);
    }
}

/// Match any of a set of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub fn any<'input, Input: 'input + Ord + PrintError>(
    set: BTreeSet<Input>,
) -> Parser<'input, impl Parse<'input, Input = Input, Output = &'input Input>> {
    satisfy_result(move |x| {
        if set.contains(x) {
            Ok(())
        } else {
            Err(format!("Expected an element of {set:#?} but found {x:#?}"))
        }
    })
}
ptest! {
    #[test]
    fn prop_any(input: u8, set: BTreeSet<u8>) {
        let slice = &[input];
        assert_eq!(set.contains(&input), any(set).parse(slice).is_ok());
    }
}

/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[derive(Debug)]
pub struct AnySeq<'platonic, Input: Ord + PrintError>(BTreeSet<&'platonic [Input]>); // TODO: reference the btree
impl<'input, Input: 'input + Ord + PrintError> Parse<'input> for AnySeq<'_, Input> {
    type Input = Input;
    type Output = &'input [Input];
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let Some((head, _)) = input.split_first() else {
            if self.0.contains(input/* [] */) {
                return Ok((input, input));
            } else {
                end_of_input!()
            }
        };
        for &option in self.0.range(core::slice::from_ref(head)..=input) {
            if let Ok((parsed, etc)) = exact_seq(option).partial(input) {
                return Ok((parsed, etc));
            }
        }
        match input.split_first() {
            Some((_, tail)) => {
                let e = &self.0;
                bail!("Expected one of {e:#?}", tail)
            }
            None => end_of_input!(),
        }
    }
}
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn any_seq<'platonic, 'input, Input: 'input + Ord + PrintError>(
    set: BTreeSet<&'platonic [Input]>,
) -> Parser<'input, AnySeq<'platonic, Input>> {
    Parser::new(AnySeq(set))
}
ptest! {
    #[test]
    fn prop_any_seq(mut input: Vec<u8>, mut aux: Vec<u8>) {
        assert_eq!(input == aux, any_seq(BTreeSet::from_iter([&*aux])).parse(&input).is_ok());
        input.append(&mut aux);
        assert_eq!(any_seq(BTreeSet::from_iter([&*input])).partial(&input).map(|(a, _)| a), Ok(&*input));
    }
}

/// Parse if possible; otherwise, don't move.
#[derive(Debug)]
pub struct Optional<'input, Fallible: Parse<'input>>(Parser<'input, Fallible>);
impl<'input, Fallible: Parse<'input>> Parse<'input> for Optional<'input, Fallible> {
    type Input = Fallible::Input;
    type Output = Option<Fallible::Output>;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        Ok(self
            .0
            .partial(input)
            .map_or_else(|_| (None, input), |(parsed, etc)| (Some(parsed), etc)))
    }
}
/// Parse if possible; otherwise, don't move.
#[inline(always)]
#[must_use]
pub const fn optional<'input, Fallible: Parse<'input>>(
    fallible: Parser<'input, Fallible>,
) -> Parser<'input, Optional<'input, Fallible>> {
    Parser::new(Optional(fallible))
}
ptest! {
    #[test]
    #[allow(clippy::unwrap_used)]
    fn prop_optional(input: [u8; 1]) {
        let parser = optional(lowercase());
        let c = input.first().unwrap();
        if c.is_ascii_lowercase() {
            assert_eq!(parser.partial(&input), Ok((Some(c), &[][..])));
        } else {
            assert_eq!(parser.partial(&input), Ok((None, &input[..])));
        }
    }
}

/// Continue parsing while a predicate holds. Return a contiguous slice referencing the section of input that held.
#[derive(Debug)]
pub struct ParseWhile<Input: PrintError, Predicate: Fn(&Input) -> bool>(
    Predicate,
    PhantomData<Input>,
);
impl<'input, Input: 'input + PrintError, Predicate: Fn(&Input) -> bool> Parse<'input>
    for ParseWhile<Input, Predicate>
{
    type Input = Input;
    type Output = &'input [Input];
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        for (i, x) in input.iter().enumerate() {
            if !self.0(x) {
                return Ok(input.split_at(i));
            }
        }
        Ok((input, &[]))
    }
}
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn parse_while<'input, Input: 'input + PrintError, Predicate: Fn(&Input) -> bool>(
    predicate: Predicate,
) -> Parser<'input, ParseWhile<Input, Predicate>> {
    Parser::new(ParseWhile(predicate, PhantomData))
}
ptest! {
    #[test]
    #[allow(clippy::unwrap_used)]
    fn prop_parse_while(input: Vec<u8>, threshold: u8) {
        let prop = move |x: &u8| x < &threshold;
        let (pass, etc) = parse_while(prop).partial(&input).unwrap();
        for x in pass {
            assert!(prop(x));
        }
        if let Some(x) = etc.first() {
            assert!(!prop(x));
        }
    }
}

/// Continue parsing as long as we can. Return a contiguous slice referencing the section of input that successfully parsed.
#[derive(Debug)]
pub struct Runaway<'input, Each: Parse<'input>>(Parser<'input, Each>);
impl<'input, Each: Parse<'input>> Parse<'input> for Runaway<'input, Each> {
    type Input = Each::Input;
    type Output = Vec<Each::Output>;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let mut results = vec![];
        let mut remaining = input;
        while let Ok((parsed, etc)) = self.0.partial(remaining) {
            results.push(parsed);
            remaining = etc;
        }
        Ok((results, remaining))
    }
}
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn runaway<'input, Each: Parse<'input>>(
    each: Parser<'input, Each>,
) -> Parser<'input, Runaway<'input, Each>> {
    Parser::new(Runaway(each))
}
ptest! {
    #[test]
    #[allow(clippy::unwrap_used)]
    fn prop_runaway(input: Vec<u8>) {
        let (pass, etc) = runaway(lowercase()).partial(&input).unwrap();
        for x in pass {
            assert!(x.is_ascii_lowercase());
        }
        if let Some(x) = etc.first() {
            assert!(!x.is_ascii_lowercase());
        }
    }
}

/// Match zero or more whitespace characters and return a reference to their contiguous slice.
#[inline(always)]
#[must_use]
pub fn whitespace<'input>() -> Parser<'input, impl Parse<'input, Input = u8>> {
    parse_while(|c| matches!(c, &b' ' | &b'\t' | &b'\r' | &b'\n'))
}
// any test would follow tautologically from the definition of `parse_while`

/// Match any lowercase letter and return a reference to it.
#[inline(always)]
#[must_use]
pub fn lowercase<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = &'input u8>> {
    satisfy(u8::is_ascii_lowercase, |x| {
        format!(
            "Expected a lowercase letter but found '{:}'",
            char::from(*x)
        )
    })
}
ptest! {
    #[test]
    fn prop_lowercase(input: u8) {
        let slice = &[input];
        assert_eq!(lowercase().partial(slice).is_ok(), input.is_ascii_lowercase());
    }
}

/// Match any uppercase letter and return a reference to it.
#[inline(always)]
#[must_use]
pub fn uppercase<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = &'input u8>> {
    satisfy(u8::is_ascii_uppercase, |x| {
        format!(
            "Expected an uppercase letter but found '{:}'",
            char::from(*x)
        )
    })
}
ptest! {
    #[test]
    fn prop_uppercase(input: u8) {
        let slice = &[input];
        assert_eq!(uppercase().partial(slice).is_ok(), input.is_ascii_uppercase());
    }
}

/// Parse a punctuated (e.g. comma-separated) list of elements determined by the parsers passed in here.
#[derive(Debug)]
pub struct Punctuated<'input, Element: Parse<'input>, Punct: Parse<'input, Input = Element::Input>>
{
    element: Parser<'input, Element>,
    punct: Parser<'input, Punct>,
    allow_trailing: bool,
}
impl<'input, Element: Parse<'input>, Punct: Parse<'input, Input = Element::Input>> Parse<'input>
    for Punctuated<'input, Element, Punct>
{
    type Input = Punct::Input;
    type Output = Vec<Element::Output>;
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (first, mut remaining) = self.element.partial(input)?;
        let mut results = vec![first];
        while let Ok(Ok((out, etc))) = {
            self.punct
                .partial(remaining)
                .map(|(_, etc)| self.element.partial(etc))
        } {
            results.push(out);
            remaining = etc;
        }
        if self.allow_trailing {
            if let Ok((_, after_trailing)) = self.punct.partial(remaining) {
                remaining = after_trailing;
            }
        }
        Ok((results, remaining))
    }
}
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn punctuated<
    'input,
    Element: Parse<'input>,
    Punct: Parse<'input, Input = Element::Input>,
>(
    element: Parser<'input, Element>,
    punct: Parser<'input, Punct>,
    allow_trailing: bool,
) -> Parser<'input, Punctuated<'input, Element, Punct>> {
    Parser::new(Punctuated {
        element,
        punct,
        allow_trailing,
    })
}

// test covered in src/tests.rs
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub fn comma_separated<'input, Element: Parse<'input, Input = u8>>(
    element: Parser<'input, Element>,
    allow_trailing: bool,
) -> Parser<
    'input,
    DiscardLeft<
        'input,
        impl Parse<'input, Input = u8>,
        Punctuated<
            'input,
            DiscardRight<'input, Element, impl Parse<'input, Input = u8>>,
            impl Parse<'input, Input = u8>,
        >,
    >,
> {
    whitespace()
        >> punctuated(
            element << whitespace(),
            exact(&b',') << whitespace(),
            allow_trailing,
        )
}

/// Parse a punctuated (e.g. by mathematical operators) list of elements determined by the parsers passed in here.
#[derive(Debug)]
pub struct PunctuatedMeaningfully<
    'input,
    Element: Parse<'input>,
    Punct: Parse<'input, Input = Element::Input>,
> {
    element: Parser<'input, Element>,
    punct: Parser<'input, Punct>,
    allow_trailing: bool,
}
impl<'input, Element: Parse<'input>, Punct: Parse<'input, Input = Element::Input>> Parse<'input>
    for PunctuatedMeaningfully<'input, Element, Punct>
{
    type Input = Punct::Input;
    type Output = (Element::Output, Vec<(Punct::Output, Element::Output)>);
    #[inline(always)]
    fn parse(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        let (head, mut remaining) = self.element.partial(input)?;
        let mut tail = vec![];
        while let Ok((p, Ok((e, etc)))) = self
            .punct
            .partial(remaining)
            .map(|(parsed, etc)| (parsed, self.element.partial(etc)))
        {
            tail.push((p, e));
            remaining = etc;
        }
        if self.allow_trailing {
            if let Ok((_, after_trailing)) = self.punct.partial(remaining) {
                remaining = after_trailing;
            }
        }
        Ok(((head, tail), remaining))
    }
}
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn punctuated_meaningfully<
    'input,
    Element: Parse<'input>,
    Punct: Parse<'input, Input = Element::Input>,
>(
    element: Parser<'input, Element>,
    punct: Parser<'input, Punct>,
    allow_trailing: bool,
) -> Parser<'input, PunctuatedMeaningfully<'input, Element, Punct>> {
    Parser::new(PunctuatedMeaningfully {
        element,
        punct,
        allow_trailing,
    })
}

/// Match a `snake_case` term.
#[inline(always)]
#[must_use]
pub fn snake_case<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = &'input [u8]>>
{
    parse_while(|x| {
        (lowercase() | exact(&b'_'))
            .partial(core::slice::from_ref(x))
            .is_ok()
    })
}
ptest! {
    #[test]
    fn prop_snake_case(input: Vec<u8>) {
        let filtered = input.into_iter().filter(|x| matches!(x, &(b'a'..=b'z') | &b'_')).collect::<Vec<_>>();
        assert_eq!(snake_case().parse(&filtered), Ok(&*filtered));
    }
}

/// Match a `SCREAMING_CASE` term.
#[inline(always)]
#[must_use]
pub fn screaming_case<'input>(
) -> Parser<'input, impl Parse<'input, Input = u8, Output = &'input [u8]>> {
    parse_while(|x| {
        (uppercase() | exact(&b'_'))
            .partial(core::slice::from_ref(x))
            .is_ok()
    })
}
ptest! {
    #[test]
    fn prop_screaming_case(input: Vec<u8>) {
        let filtered = input.into_iter().filter(|x| matches!(x, &(b'A'..=b'Z') | &b'_')).collect::<Vec<_>>();
        assert_eq!(screaming_case().parse(&filtered), Ok(&*filtered));
    }
}

/// Match a single digit.
#[inline(always)]
#[must_use]
pub fn digit<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = u8>> {
    satisfy(u8::is_ascii_digit, |x| {
        format!(
            "Expected an uppercase letter but found '{:}' (ASCII #{x:})",
            char::from(*x)
        )
    })
    .pipe(|x| {
        x.checked_sub(b'0').ok_or_else(|| {
            format!(
                "Found an ASCII digit but couldn't subtract '0' from it. Digit was '{:}' (ASCII #{x:}).",
                char::from(*x)
            )
        })
    })
}

/// Match a base-ten unsigned integer.
#[inline(always)]
#[must_use]
pub fn unsigned_integer<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = usize>>
{
    runaway(digit()).pipe(move |s| {
        s.into_iter().fold(Ok(0), |acc, digit| {
            acc.map_or_else(
                |e| Err(e),
                |x: usize| match x.checked_mul(10).map(|x0| x0.checked_add(digit.into())) {
                    Some(Some(y)) => Ok(y),
                    _ => Err(String::from(
                        "Parsing error: parsed integer that would overflow a Rust `usize`",
                    )),
                },
            )
        })
    })
}

/// Match a base-ten signed integer.
#[inline(always)]
#[must_use]
pub fn signed_integer<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = isize>> {
    (optional(exact(&b'-'))
        & runaway(digit()).pipe(move |s| {
            s.into_iter().fold(Ok(0), |acc, digit| {
                acc.map_or_else(
                    |e| Err(e),
                    |x: isize| match x.checked_mul(10).map(|x0| x0.checked_add(digit.into())) {
                        Some(Some(y)) => Ok(y),
                        _ => Err(String::from(
                            "Parsing error: parsed integer that would overflow a Rust `isize`",
                        )),
                    },
                )
            })
        }))
    .pipe(|(maybe_neg, abs)| {
        if maybe_neg.is_some() {
            abs.checked_neg().ok_or_else(|| {
                String::from(
                    "Parsing error: parsed a negative integer that would overflow a Rust `isize`",
                )
            })
        } else {
            Ok(abs)
        }
    })
}

/*

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
    /// Match a single digit and return a reference to it (as an integer, not a character).
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
/// If you can match, return a reference to it; if not, stay in the same place.
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
/// If you can match, return a reference to it; if not, stay in the same place.
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

*/

/// Operator-precedence parsing, e.g. for infix math.
pub mod precedence {
    #[allow(clippy::wildcard_imports)]
    use super::*;

    /// Parse binary operations without grouping them by precedence into a tree: just return a list of operators and things to the right of them.
    pub fn raw_binary_ops<'platonic, 'input, Primary: Parse<'input, Input = u8>>(
        primary: Parser<'input, Primary>,
        ops: BTreeSet<&'platonic [u8]>,
    ) -> Parser<
        'input,
        PunctuatedMeaningfully<
            'input,
            DiscardRight<'input, Primary, impl Parse<'input, Input = u8>>,
            DiscardRight<'input, AnySeq<'platonic, u8>, impl Parse<'input, Input = u8>>,
        >,
    > {
        punctuated_meaningfully(primary << whitespace(), any_seq(ops) << whitespace(), false)
    }
}
