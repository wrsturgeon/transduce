/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#[allow(clippy::wildcard_imports)]
use crate::base::*;

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
