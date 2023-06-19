/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#[allow(clippy::wildcard_imports)]
use crate::base::*;

#[cfg(feature = "alloc")]
use crate::{Parse, Parser};

#[cfg(feature = "alloc")]
use alloc::{format, string::String, vec /* the macro */, vec::Vec};

#[test]
fn a_to_b() {
    let parser = {
        whitespace() >> anything() << whitespace() << exact(&b'-') << exact(&b'>') << whitespace()
            & anything() << whitespace() << end()
    };
    assert_eq!(parser.parse(b"A -> B"), Ok((&b'A', &b'B')));
    assert_eq!(parser.parse(b"A->B"), Ok((&b'A', &b'B')));
    assert_eq!(parser.parse(b"  A   ->  B     "), Ok((&b'A', &b'B')));
}

#[cfg(feature = "alloc")]
#[test]
fn comma_separated_alphabet() {
    let parser = comma_separated(anything(), true);
    assert_eq!(parser.partial(b"a"), Ok((vec![&b'a'], &[][..])));
    assert_eq!(
        parser.partial(b"   a, b, c,d,e,f,        g    ,    h , i       ,   j , k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z     ,      ").map(|(v, etc)| (v.into_iter().map(|x| char::from(*x)).collect(), etc)),
        Ok((('a'..='z').collect::<Vec<_>>(), &*vec![])),
    );
}

#[cfg(feature = "alloc")]
#[derive(Debug, PartialEq)]
enum Literal {
    Character(u8),
    Digit(u8),
}

#[cfg(feature = "alloc")]
fn literal<'input>() -> Parser<'input, impl Parse<'input, Input = u8, Output = Literal>> {
    #![allow(clippy::arithmetic_side_effects)]
    ((exact(&b'\'') >> (lowercase() | uppercase()) << exact(&b'\''))
        .pipe(|x| Ok(Literal::Character(*x))))
        | (digit().pipe(|x| Ok(Literal::Digit(x))))
}

#[cfg(feature = "alloc")]
#[test]
fn literals() {
    let parser = exact(&b'(') >> comma_separated(literal(), true) << exact(&b')');
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

#[test]
fn optional_zero() {
    #![allow(clippy::assertions_on_result_states)]
    let parser = optional(exact(&b'0')) >> exact(&b'1') & exact(&b'2') & exact(&b'3');
    assert_eq!(parser.parse(b"123"), Ok(((&b'1', &b'2'), &b'3')));
    assert_eq!(parser.parse(b"0123"), Ok(((&b'1', &b'2'), &b'3')));
    assert!(parser.parse(b"00123").is_err());
}

mod failures {
    #![allow(
        clippy::let_underscore_untyped,
        clippy::wildcard_imports,
        clippy::let_underscore_must_use
    )]

    use super::*;

    #[should_panic]
    #[test]
    fn fail() {
        let _ = (anything() << exact(&b'!')).parse_or_panic(b"???");
    }

    #[should_panic]
    #[test]
    fn not_everything() {
        let _ = (exact(&b'?') >> exact(&b'?')).parse_or_panic(b"???");
    }

    #[should_panic]
    #[test]
    fn oob() {
        let _ = (anything() & anything() & anything() & anything()).parse_or_panic(b"???");
    }

    #[should_panic]
    #[test]
    fn not_expecting_a_newline() {
        let _ = (anything() << exact(&b'!')).parse_or_panic(b"?\n?\n?");
    }

    #[should_panic]
    #[test]
    fn multiline_fail() {
        let _ = (anything() << whitespace() << exact(&b'!')).parse_or_panic(b"?\n?\n?");
    }

    #[should_panic]
    #[test]
    fn multiline_not_everything() {
        let _ = (exact(&b'?') << whitespace() >> exact(&b'?')).parse_or_panic(b"?\n?\n?");
    }

    #[should_panic]
    #[test]
    fn multiline_oob() {
        let _ = (anything() << whitespace()
            & anything() << whitespace()
            & anything() << whitespace()
            & anything())
        .parse_or_panic(b"?\n?\n?");
    }

    #[cfg(feature = "alloc")]
    mod msg {
        use super::*;

        #[test]
        fn fail_msg() {
            assert_eq!(
                (anything() << exact(&b'!')).parse(b"???"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 1 | ?\u{1b}[0;1;41m?\u{1b}[0m?\r\n",
                    "   |  \u{1b}[0;1;31m^ Expected 33 but found 63\u{1b}[0m\r\n",
                )))
            );
        }

        #[test]
        fn not_everything_msg() {
            assert_eq!(
                (exact(&b'?') << exact(&b'?')).parse(b"???"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 1 | ??\u{1b}[0;1;41m?\u{1b}[0m\r\n",
                    "   |   \u{1b}[0;1;31m^ Unparsed input remains after parsing what should have been everything\u{1b}[0m\r\n",
                )))
            );
        }

        #[test]
        fn oob_msg() {
            assert_eq!(
                (anything() & anything() & anything() & anything()).parse(b"???"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 1 | ???\u{1b}[0;1;41m \u{1b}[0m\r\n",
                    "   |    \u{1b}[0;1;31m^ Reached end of input but expected an item\u{1b}[0m\r\n",
                )))
            );
        }

        #[test]
        fn not_expecting_a_newline_msg() {
            assert_eq!(
                (anything() << exact(&b'!')).parse(b"?\n?\n?"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 1 | ?\u{1b}[0;1;41m \u{1b}[0m\r\n",
                    "   |  \u{1b}[0;1;31m^ Expected 33 but found 10\u{1b}[0m\r\n",
                    " 2 | ?\r\n",
                )))
            );
        }

        #[test]
        fn multiline_fail_msg() {
            assert_eq!(
                (anything() << whitespace() << exact(&b'!')).parse(b"?\n?\n?"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 1 | ?\r\n",
                    " 2 | \u{1b}[0;1;41m?\u{1b}[0m\r\n",
                    "   | \u{1b}[0;1;31m^ Expected 33 but found 63\u{1b}[0m\r\n",
                    " 3 | ?\r\n",
                )))
            );
        }

        #[test]
        fn multiline_not_everything_msg() {
            assert_eq!(
                (exact(&b'?') << whitespace() >> exact(&b'?')).parse(b"?\n?\n?"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 1 | ?\r\n",
                    " 2 | ?\u{1b}[0;1;41m \u{1b}[0m\r\n",
                    "   |  \u{1b}[0;1;31m^ Unparsed input remains after parsing what should have been everything\u{1b}[0m\r\n",
                    " 3 | ?\r\n",
                )))
            );
        }

        #[test]
        fn multiline_oob_msg() {
            assert_eq!(
                (anything() << whitespace()
                    & anything() << whitespace()
                    & anything() << whitespace()
                    & anything())
                .parse(b"?\n?\n?"),
                Err(String::from(concat!(
                    "\r\n",
                    "   | \u{1b}[0;1;31mError while parsing:\u{1b}[0m\r\n",
                    " 2 | ?\r\n",
                    " 3 | ?\u{1b}[0;1;41m \u{1b}[0m\r\n",
                    "   |  \u{1b}[0;1;31m^ Reached end of input but expected an item\u{1b}[0m\r\n",
                )))
            );
        }
    }
}

#[cfg(feature = "alloc")]
#[test]
fn binops_without_precedence() {
    assert_eq!(
        precedence::raw_binary_ops(
            lowercase(),
            ::alloc::collections::BTreeSet::from_iter([&b"+"[..], b"-", b"*", b"/"])
        )
        .partial(b"a + b - c * d / e"),
        Ok((
            (
                &b'a',
                vec![
                    (&b"+"[..], &b'b'),
                    (b"-", &b'c'),
                    (b"*", &b'd'),
                    (b"/", &b'e')
                ]
            ),
            &[][..]
        ))
    );
}

#[cfg(feature = "alloc")]
proptest::proptest! {
    #[test]
    fn prop_unsigned_int(i: usize) {
        let fmt = format!("{i:}");
        let p = unsigned_integer().parse_or_panic(fmt.as_bytes());
        assert_eq!(p, i);
    }

    #[test]
    fn prop_signed_int(i: isize) {
        let fmt = format!("{i:}");
        let p = signed_integer().parse_or_panic(fmt.as_bytes());
        assert_eq!(p, i);
    }
}

#[cfg(feature = "alloc")]
#[test]
fn parse_huge_ints() {
    #![allow(clippy::as_conversions, clippy::assertions_on_result_states)]
    const SMALLER: usize = usize::MAX;
    const LARGER: u128 = (SMALLER as u128).overflowing_add(1).0;
    let fmt_smaller = format!("{SMALLER:}");
    let fmt_larger = format!("{LARGER:}");
    assert_eq!(
        unsigned_integer().parse(fmt_smaller.as_bytes()),
        Ok(SMALLER)
    );
    if core::mem::size_of::<usize>() >= core::mem::size_of::<u128>() {
        return; // nothing we can do on this machine
    }
    assert!(unsigned_integer().parse(fmt_larger.as_bytes()).is_err());
}
