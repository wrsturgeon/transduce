/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#[allow(clippy::wildcard_imports)]
use crate::{base::*, Read};

#[test]
fn a_to_b() {
    let parser = move || {
        whitespace() >> verbatim() << whitespace() << exact(b'-') << exact(b'>') << whitespace()
            & verbatim() << whitespace() << end()
    };
    assert_eq!(parser().parse(b"A -> B"), Ok((b'A', b'B')));
    assert_eq!(parser().parse(b"A->B"), Ok((b'A', b'B')));
    assert_eq!(parser().parse(b"  A   ->  B     "), Ok((b'A', b'B')));
}

#[test]
fn comma_separated_alphabet() {
    assert_eq!(
        comma_separated(verbatim).0(b"a"),
        Ok((vec![b'a'], &*vec![]))
    );
    assert_eq!(
        comma_separated(verbatim).0(b"   a, b, c,d,e,f,        g    ,    h , i       ,   j , k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z     ,      "),
        Ok(((b'a'..=b'z').collect::<Vec<_>>(), &*vec![])),
    );
}

#[derive(Debug, PartialEq)]
enum Literal {
    Character(u8),
    Digit(u8),
}

impl Read for Literal {
    #[cfg(feature = "nightly")]
    #[inline(always)]
    #[must_use]
    fn parser(
    ) -> crate::Parser<u8, Self, impl FnOnce(&[u8]) -> crate::result::Result<(Self, &[u8])>> {
        exact(b'\'') >> (lowercase() | uppercase()) << exact(b'\'') ^ Literal::Character
            | digit() ^ Literal::Digit
    }
    #[cfg(not(feature = "nightly"))]
    #[inline(always)]
    #[must_use]
    fn parser() -> crate::Parser<u8, Self> {
        exact(b'\'') >> (lowercase() | uppercase()) << exact(b'\'') ^ Literal::Character
            | digit() ^ Literal::Digit
    }
}

#[test]
fn literals() {
    let parser = exact(b'(') >> comma_separated(Literal::parser) << exact(b')');
    assert_eq!(
        parser.parse(
            b"('a', 0, 'b', 1, 'c', 2, 'd', 3, 'e', 4, 'f', 5, 'g', 6, 'h', 7, 'i', 8, 'j', 9,)"
        ),
        Ok(vec![
            Literal::Character(b'a'),
            Literal::Digit(0),
            Literal::Character(b'b'),
            Literal::Digit(1),
            Literal::Character(b'c'),
            Literal::Digit(2),
            Literal::Character(b'd'),
            Literal::Digit(3),
            Literal::Character(b'e'),
            Literal::Digit(4),
            Literal::Character(b'f'),
            Literal::Digit(5),
            Literal::Character(b'g'),
            Literal::Digit(6),
            Literal::Character(b'h'),
            Literal::Digit(7),
            Literal::Character(b'i'),
            Literal::Digit(8),
            Literal::Character(b'j'),
            Literal::Digit(9),
        ])
    );
}
