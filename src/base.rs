/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! Common functions to drop in instead of reinventing the wheel.

#![allow(clippy::tests_outside_test_module, clippy::wildcard_imports)]

use crate::{print_error::PrintError, *};
use core::marker::PhantomData;

#[cfg(any(feature = "alloc", test))]
#[cfg_attr(test, allow(unused_imports))]
use alloc::{format, string::String, vec /* the macro */, vec::Vec};

/// Representation of a set. O(lg n)-search BST when allocation is available, otherwise an O(n) slice.
#[cfg(feature = "alloc")]
#[allow(clippy::single_char_lifetime_names)]
type Set<'a, T> = alloc::collections::BTreeSet<T>;

/// Representation of a set. O(lg n)-search BST when allocation is available, otherwise an O(n) slice.
#[cfg(not(feature = "alloc"))]
#[allow(clippy::single_char_lifetime_names)]
type Set<'a, T> = &'a [T];

/// Property test active only when testing (not in debug or release builds).
macro_rules! ptest {
    ($($tt:tt)*) => {
        #[cfg(test)]
        ::proptest::proptest! { $($tt)* }
    };
}

#[cfg(feature = "alloc")]
/// End-of-input error: expected an item but none remaining.
macro_rules! end_of_input {
    () => {
        return Err(ParseError {
            message: String::from("Reached end of input but expected an item"),
            not_yet_parsed: None,
        })
    };
}

#[cfg(not(feature = "alloc"))]
/// End-of-input error: expected an item but none remaining.
macro_rules! end_of_input {
    () => {
        return Err(ParseError {
            message: "Reached end of input but expected an item",
            not_yet_parsed: None,
        })
    };
}

// TODO: const
/// Return anything that does _not_ satisfy this predicate.
#[inline(always)]
#[must_use]
pub fn not<
    'input,
    Input: 'input + PrintError,
    Predicate: Fn(&'input Input) -> bool,
    WriteMessage: Fn(&'input Input) -> Message,
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
        assert_eq!(pred(&input), not(pred, |_| string_if_alloc("")).parse(slice).is_err());
    }
}

/// Match any single item and return a reference to it.
#[derive(Debug)]
pub struct Anything<Input: PrintError>(PhantomData<Input>);
impl<'input, Input: 'input + PrintError> Parse<'input> for Anything<Input> {
    type Input = Input;
    type Output = &'input Input;
    #[inline(always)]
    fn partial(
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

/// Make sure we've reached exactly the end of the stream of input; don't advance to it.
#[derive(Debug)]
pub struct End<Input: PrintError>(PhantomData<Input>);
impl<'input, Input: 'input + PrintError> Parse<'input> for End<Input> {
    type Input = Input;
    type Output = ();
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        if !input.is_empty() {
            bail!(
                "Expected the end of the input stream but found input remaining",
                "Expected the end of the input stream but found input remaining",
                input,
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

// TODO: const (nightly?)
/// Skip zero or more items while this predicate holds on them.
#[inline(always)]
#[must_use]
pub fn satisfy<
    'input,
    Input: 'input + PrintError,
    Predicate: Fn(&'input Input) -> bool,
    WriteMessage: Fn(&'input Input) -> Message,
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
        assert_eq!(pred(&input), satisfy(pred, |_| string_if_alloc("")).parse(slice).is_ok());
    }
}

// TODO: const (nightly?)
/// Skip zero or more items while this predicate holds on them.
#[inline(always)]
#[must_use]
pub fn satisfy_result<
    'input,
    Input: 'input + PrintError,
    Predicate: Fn(&'input Input) -> Result<(), Message>,
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
        assert_eq!(pred(&input), satisfy_result(move |x| if pred(x) { Ok(()) } else { Err(string_if_alloc("")) }).parse(slice).is_ok());
    }
}

// TODO: const
/// Match exactly this item and return a reference to the original (not the parsed one).
#[inline(always)]
#[must_use]
pub fn exact<Input: PartialEq<Input> + PrintError>(
    reference: &'_ Input,
) -> Parser<'_, impl Parse<'_, Input = Input, Output = &'_ Input>> {
    #![cfg_attr(not(feature = "alloc"), allow(unused_variables))]
    satisfy(
        move |x| reference.eq(x),
        move |head| {
            format_if_alloc!(
                "Expected one exact item but found another",
                "Expected {reference:#?} but found {head:#?}"
            )
        },
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
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        #![cfg_attr(not(feature = "alloc"), allow(unused_variables))]
        let mut etc = input;
        for i in self.0 {
            match etc.split_first() {
                None => end_of_input!(),
                Some((head, tail)) => {
                    if head != i {
                        let e = self.0;
                        bail!(
                            "Expected {e:#?} but found {head:#?} instead of {i:#?}",
                            "Expected one character but found another",
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
    set: Set<'input, Input>,
) -> Parser<'input, impl Parse<'input, Input = Input, Output = &'input Input>> {
    satisfy_result(move |x| {
        if set.contains(x) {
            Ok(())
        } else {
            Err(format_if_alloc!(
                "Expected one of a set of single items but the actual item was not one of them",
                "Expected an element of {set:#?} but found {x:#?}"
            ))
        }
    })
}
ptest! {
    #[test]
    fn prop_any(input: u8, set: alloc::collections::BTreeSet<u8>) {
        let slice = &[input];
        #[cfg(feature="alloc")]
        assert_eq!(set.contains(&input), any(set).parse(slice).is_ok());
        #[cfg(not(feature="alloc"))]
        {
            let flat: Vec<_> = set.iter().copied().collect();
            assert_eq!(set.contains(&input), any(&flat).parse(slice).is_ok());
        }
    }
}

/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[derive(Debug)]
pub struct AnySeq<'platonic, Input: Ord + PrintError>(Set<'platonic, &'platonic [Input]>); // TODO: reference the btree
impl<'input, Input: 'input + Ord + PrintError> Parse<'input> for AnySeq<'_, Input> {
    type Input = Input;
    type Output = &'input [Input];
    #[inline(always)]
    fn partial(
        &self,
        input: &'input [Self::Input],
    ) -> Result<(Self::Output, &'input [Self::Input]), ParseError> {
        #![cfg_attr(not(feature = "alloc"), allow(unused_variables))]
        let Some((head, _)) = input.split_first() else {
            if self.0.contains(&input/* [] */) {
                return Ok((input, input));
            }
            end_of_input!()
        };
        // With `alloc`, O(lg n)
        #[cfg(feature = "alloc")]
        for &option in self.0.range(core::slice::from_ref(head)..=input) {
            if let Ok((parsed, etc)) = exact_seq(option).partial(input) {
                return Ok((parsed, etc));
            }
        }
        // Without, O(n)
        #[cfg(not(feature = "alloc"))]
        for &option in self.0 {
            if let Ok((parsed, etc)) = exact_seq(option).partial(input) {
                return Ok((parsed, etc));
            }
        }
        match input.split_first() {
            Some((_, tail)) => {
                let e = &self.0;
                bail!(
                    "Expected one of {e:#?}",
                    "Expected a sequence of characters but found another",
                    tail
                )
            }
            None => end_of_input!(),
        }
    }
}
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn any_seq<'platonic, 'input, Input: 'input + Ord + PrintError>(
    set: Set<'platonic, &'platonic [Input]>,
) -> Parser<'input, AnySeq<'platonic, Input>> {
    Parser::new(AnySeq(set))
}
ptest! {
    #[test]
    fn prop_any_seq(mut input: Vec<u8>, mut aux: Vec<u8>) {
        #[cfg(feature = "alloc")]
        assert_eq!(input == aux, any_seq(Set::from_iter([&*aux])).parse(&input).is_ok());
        #[cfg(not(feature = "alloc"))]
        assert_eq!(input == aux, any_seq(&[&aux]).parse(&input).is_ok());
        input.append(&mut aux);
        #[cfg(feature = "alloc")]
        assert_eq!(any_seq(Set::from_iter([&*input])).partial(&input).map(|(a, _)| a), Ok(&*input));
        #[cfg(not(feature = "alloc"))]
        assert_eq!(any_seq(&[&input]).partial(&input).map(|(a, _)| a), Ok(&*input));
    }
}

/// Parse if possible; otherwise, don't move.
#[derive(Debug)]
pub struct Optional<'input, Fallible: Parse<'input>>(Parser<'input, Fallible>);
impl<'input, Fallible: Parse<'input>> Parse<'input> for Optional<'input, Fallible> {
    type Input = Fallible::Input;
    type Output = Option<Fallible::Output>;
    #[inline(always)]
    fn partial(
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
    fn partial(
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

// TODO: enable without `alloc` with a maximum length
#[cfg(feature = "alloc")]
/// Continue parsing as long as we can. Return a contiguous slice referencing the section of input that successfully parsed.
#[derive(Debug)]
pub struct Runaway<'input, Each: Parse<'input>>(Parser<'input, Each>);
#[cfg(feature = "alloc")]
impl<'input, Each: Parse<'input>> Parse<'input> for Runaway<'input, Each> {
    type Input = Each::Input;
    type Output = Vec<Each::Output>;
    #[inline(always)]
    fn partial(
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
#[cfg(feature = "alloc")]
/// Match any of a set of sequences of options. Set represented as a binary tree for efficient lookup.
#[inline(always)]
#[must_use]
pub const fn runaway<'input, Each: Parse<'input>>(
    each: Parser<'input, Each>,
) -> Parser<'input, Runaway<'input, Each>> {
    Parser::new(Runaway(each))
}
#[cfg(feature = "alloc")]
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
pub fn maybe_space<'input>() -> Parser<'input, impl Parse<'input, Input = u8>> {
    parse_while(u8::is_ascii_whitespace)
}
// any test would follow tautologically from the definition of `parse_while`

/// Match one or more whitespace characters and return a reference to their contiguous slice.
#[inline(always)]
#[must_use]
pub fn need_space<'input>() -> Parser<'input, impl Parse<'input, Input = u8>> {
    satisfy(u8::is_ascii_whitespace, |_| {
        String::from("Expected whitespace")
    }) >> parse_while(u8::is_ascii_whitespace)
}
// any test would follow tautologically from the definition of `parse_while`

/// Match any lowercase letter and return a reference to it.
#[inline(always)]
#[must_use]
pub fn lowercase<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = &'input u8>> {
    #![cfg_attr(not(feature = "alloc"), allow(unused_variables))]
    satisfy(u8::is_ascii_lowercase, |x| {
        format_if_alloc!(
            "Expected a lowercase letter",
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
    #![cfg_attr(not(feature = "alloc"), allow(unused_variables))]
    satisfy(u8::is_ascii_uppercase, |x| {
        format_if_alloc!(
            "Expected an uppercase letter",
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

#[cfg(feature = "alloc")]
/// Parse a punctuated (e.g. comma-separated) list of elements determined by the parsers passed in here.
#[derive(Debug)]
pub struct Punctuated<'input, Element: Parse<'input>, Punct: Parse<'input, Input = Element::Input>>
{
    /// Parser for each element.
    element: Parser<'input, Element>,
    /// Parser for punctuation.
    punct: Parser<'input, Punct>,
    /// Whether to allow trailing punctuation, e.g. `a, b, c,` instead of only `a, b, c`.
    allow_trailing: bool,
}
#[cfg(feature = "alloc")]
impl<'input, Element: Parse<'input>, Punct: Parse<'input, Input = Element::Input>> Parse<'input>
    for Punctuated<'input, Element, Punct>
{
    type Input = Punct::Input;
    type Output = Vec<Element::Output>;
    #[inline(always)]
    fn partial(
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
#[cfg(feature = "alloc")]
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

#[cfg(feature = "alloc")]
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
    #![allow(clippy::arithmetic_side_effects)]
    maybe_space()
        >> punctuated(
            element << maybe_space(),
            exact(&b',') << maybe_space(),
            allow_trailing,
        )
}

#[cfg(feature = "alloc")]
/// Parse a punctuated (e.g. by mathematical operators) list of elements determined by the parsers passed in here.
#[derive(Debug)]
pub struct PunctuatedMeaningfully<
    'input,
    Element: Parse<'input>,
    Punct: Parse<'input, Input = Element::Input>,
> {
    /// Parser for each element.
    element: Parser<'input, Element>,
    /// Parser for punctuation.
    punct: Parser<'input, Punct>,
    /// Whether to allow trailing punctuation, e.g. `a, b, c,` instead of only `a, b, c`.
    allow_trailing: bool,
}
#[cfg(feature = "alloc")]
impl<'input, Element: Parse<'input>, Punct: Parse<'input, Input = Element::Input>> Parse<'input>
    for PunctuatedMeaningfully<'input, Element, Punct>
{
    type Input = Punct::Input;
    type Output = (Element::Output, Vec<(Punct::Output, Element::Output)>);
    #[inline(always)]
    fn partial(
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
#[cfg(feature = "alloc")]
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
    #![cfg_attr(not(feature = "alloc"), allow(unused_variables))]
    satisfy(u8::is_ascii_digit, |x| {
        format_if_alloc!(
            "Expected an uppercase letter",
            "Expected an uppercase letter but found '{:}' (ASCII #{x:})",
            char::from(*x)
        )
    })
    .pipe(|x| {
        #[allow(clippy::unnecessary_lazy_evaluations)]
        x.checked_sub(b'0').ok_or_else(|| {
            format_if_alloc!(
                "Found an ASCII digit but couldn't subtract 0 from it",
                "Found an ASCII digit but couldn't subtract '0' from it. Digit was '{:}' (ASCII #{x:}).",
                char::from(*x)
            )
        })
    })
}

#[cfg(feature = "alloc")]
/// Match a base-ten unsigned integer.
#[inline(always)]
#[must_use]
pub fn unsigned_integer<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = usize>>
{
    runaway(digit()).pipe(move |s| {
        s.into_iter().fold(Ok(0), |acc, digit| {
            acc.map_or_else(Err, |x: usize| {
                match x.checked_mul(10).map(|x0| x0.checked_add(digit.into())) {
                    Some(Some(y)) => Ok(y),
                    _ => Err(string_if_alloc(
                        "Parsing error: parsed integer that would overflow a Rust `usize`",
                    )),
                }
            })
        })
    })
}

#[cfg(feature = "alloc")]
/// Match a base-ten signed integer.
#[inline(always)]
#[must_use]
pub fn signed_integer<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = isize>> {
    (optional(exact(&b'-'))
        & runaway(digit()).pipe(move |s| {
            s.into_iter().fold(Ok(0), |acc, digit| {
                acc.map_or_else(Err, |x: isize| {
                    match x.checked_mul(10).map(|x0| x0.checked_add(digit.into())) {
                        Some(Some(y)) => Ok(y),
                        _ => Err(String::from(
                            "Parsing error: parsed integer that would overflow a Rust `isize`",
                        )),
                    }
                })
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

#[cfg(feature = "alloc")]
/// Operator-precedence parsing, e.g. for infix math.
pub mod precedence {
    #[allow(clippy::wildcard_imports)]
    use super::*;

    /// Parse binary operations without grouping them by precedence into a tree: just return a list of operators and things to the right of them.
    #[inline(always)]
    #[must_use]
    pub fn raw_binary_ops<'platonic, 'input, Primary: Parse<'input, Input = u8>>(
        primary: Parser<'input, Primary>,
        ops: Set<'platonic, &'platonic [u8]>,
    ) -> Parser<
        'input,
        PunctuatedMeaningfully<
            'input,
            DiscardRight<'input, Primary, impl Parse<'input, Input = u8>>,
            DiscardRight<'input, AnySeq<'platonic, u8>, impl Parse<'input, Input = u8>>,
        >,
    > {
        #![allow(clippy::arithmetic_side_effects)]
        punctuated_meaningfully(
            primary << maybe_space(),
            any_seq(ops) << maybe_space(),
            false,
        )
    }
}

/// Common patterns in programming languages.
pub mod lang {
    #[allow(clippy::wildcard_imports)]
    use super::*;

    /// Parse a `let some_name = expression...;`-style binding.
    #[inline(always)]
    #[must_use]
    pub fn let_expr<'input, P: Parse<'input, Input = u8>>(
        expression: Parser<'input, P>,
    ) -> Parser<
        'input,
        Both<
            'input,
            impl Parse<'input, Input = u8, Output = &'input [u8]>,
            DiscardLeft<'input, impl Parse<'input, Input = u8>, P>,
        >,
    > {
        exact_seq(b"let") >> need_space() >> snake_case()
            & maybe_space() >> exact(&b'=') >> maybe_space() >> expression
    }
}
