/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![cfg_attr(feature = "nightly", allow(dead_code, unused_imports))] // FIXME

#[allow(clippy::wildcard_imports)]
use crate::{base::*, Read};

#[test]
fn a_to_b() {
    let parser = || {
        whitespace() >> verbatim() << whitespace() << exact(&b'-') << exact(&b'>') << whitespace()
            & verbatim() << whitespace() << end()
    };
    assert_eq!(parser().parse(b"A -> B"), Ok((&b'A', &b'B')));
    assert_eq!(parser().parse(b"A->B"), Ok((&b'A', &b'B')));
    assert_eq!(parser().parse(b"  A   ->  B     "), Ok((&b'A', &b'B')));
}

#[test]
fn comma_separated_alphabet() {
    assert_eq!(
        comma_separated(verbatim).0(b"a"),
        Ok((vec![&b'a'], &*vec![]))
    );
    assert_eq!(
        comma_separated(verbatim).0(b"   a, b, c,d,e,f,        g    ,    h , i       ,   j , k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z     ,      ").map(|(v, etc)| (v.into_iter().copied().collect(), etc)),
        Ok(((b'a'..=b'z').collect::<Vec<_>>(), &*vec![])),
    );
}

#[derive(Debug, PartialEq)]
enum Literal<'a> {
    Character(&'a u8),
    Digit(u8),
}

#[cfg(not(feature = "nightly"))] // FIXME
impl<'input, 'f> Read<'input, 'f> for Literal<'input> {
    #[cfg(feature = "nightly")]
    #[inline(always)]
    #[must_use]
    fn parser() -> crate::Parser<
        'input,
        'f,
        u8,
        Literal<'input>,
        impl 'f + FnOnce(&'input [u8]) -> crate::result::Result<(Literal<'input>, &'input [u8])>,
    > {
        exact(&b'\'') >> (lowercase() | uppercase) << exact(&b'\'') ^ Literal::Character
            | || digit() ^ Literal::Digit
    }
    #[cfg(not(feature = "nightly"))]
    #[inline(always)]
    #[must_use]
    fn parser() -> crate::Parser<'input, 'f, u8, Self> {
        exact(&b'\'') >> (lowercase() | uppercase) << exact(&b'\'') ^ Literal::Character
            | || digit() ^ Literal::Digit
    }
}

#[cfg(not(feature = "nightly"))] // FIXME
#[test]
fn literals() {
    let parser = exact(&b'(') >> comma_separated(Literal::parser) << exact(&b')');
    assert_eq!(
        parser.parse(
            b"('a', 0, 'b', 1, 'c', 2, 'd', 3, 'e', 4, 'f', 5, 'g', 6, 'h', 7, 'i', 8, 'j', 9,)"
        ),
        Ok(vec![
            Literal::Character(&b'a'),
            Literal::Digit(0),
            Literal::Character(&b'b'),
            Literal::Digit(1),
            Literal::Character(&b'c'),
            Literal::Digit(2),
            Literal::Character(&b'd'),
            Literal::Digit(3),
            Literal::Character(&b'e'),
            Literal::Digit(4),
            Literal::Character(&b'f'),
            Literal::Digit(5),
            Literal::Character(&b'g'),
            Literal::Digit(6),
            Literal::Character(&b'h'),
            Literal::Digit(7),
            Literal::Character(&b'i'),
            Literal::Digit(8),
            Literal::Character(&b'j'),
            Literal::Digit(9),
        ])
    );
}

#[test]
fn optional_zero() {
    #![allow(clippy::assertions_on_result_states)]
    let parser = || optional(|| exact(&b'0')) >> exact(&b'1') & exact(&b'2') & exact(&b'3');
    assert_eq!(parser().parse(b"123"), Ok(((&b'1', &b'2'), &b'3')));
    assert_eq!(parser().parse(b"0123"), Ok(((&b'1', &b'2'), &b'3')));
    assert!(parser().parse(b"00123").is_err());
}

mod failures {
    #![allow(
        clippy::let_underscore_untyped,
        clippy::wildcard_imports,
        clippy::let_underscore_must_use
    )]

    use super::*;

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn fail() {
        let _ = (verbatim() << exact(&b'!')).parse_or_panic(b"???");
    }

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn not_everything() {
        let _ = (exact(&b'?') >> exact(&b'?')).parse_or_panic(b"???");
    }

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn oob() {
        let _ = (verbatim() & verbatim() & verbatim() & verbatim()).parse_or_panic(b"???");
    }

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn not_expecting_a_newline() {
        let _ = (verbatim() << exact(&b'!')).parse_or_panic(b"?\n?\n?");
    }

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn multiline_fail() {
        let _ = (verbatim() << whitespace() << exact(&b'!')).parse_or_panic(b"?\n?\n?");
    }

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn multiline_not_everything() {
        let _ = (exact(&b'?') << whitespace() >> exact(&b'?')).parse_or_panic(b"?\n?\n?");
    }

    #[should_panic] // Remove to see the gorgeous error message
    #[test]
    fn multiline_oob() {
        let _ = (verbatim() << whitespace()
            & verbatim() << whitespace()
            & verbatim() << whitespace()
            & verbatim())
        .parse_or_panic(b"?\n?\n?");
    }
}

#[test]
fn binops_without_precedence() {
    assert_eq!(
        precedence::raw_binops(
            lowercase,
            &::alloc::collections::BTreeSet::from_iter([
                &b"+"[..],
                &b"-"[..],
                &b"*"[..],
                &b"/"[..]
            ])
        )
        .once(b"a + b - c * d / e")
        .map(|(a, _)| a),
        Ok((
            &b'a',
            vec![
                (&b"+"[..], &b'b'),
                (b"-", &b'c'),
                (b"*", &b'd'),
                (b"/", &b'e')
            ]
        ))
    );
}

proptest::proptest! {
    #[test]
    fn prop_unsigned_int(i in usize::MIN..=usize::MAX) {
        assert_eq!(
            unsigned_integer().parse(format!("{i:}").as_bytes()),
            Ok(i),
        );
    }

    #[test]
    fn prop_signed_int(i in isize::MIN..=isize::MAX) {
        assert_eq!(
            signed_integer().parse(format!("{i:}").as_bytes()),
            Ok(i),
        );
    }
}

#[test]
fn parse_huge_ints() {
    #![allow(clippy::as_conversions, clippy::assertions_on_result_states)]
    const SMALLER: usize = usize::MAX;
    const LARGER: u128 = (SMALLER as u128).overflowing_add(1).0;
    assert_eq!(
        unsigned_integer().parse(format!("{SMALLER:}").as_bytes()),
        Ok(SMALLER)
    );
    if core::mem::size_of::<usize>() >= core::mem::size_of::<u128>() {
        return; // nothing we can do on this machine
    }
    assert!(unsigned_integer()
        .parse(format!("{LARGER:}").as_bytes())
        .is_err());
}
