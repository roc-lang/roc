# META
~~~ini
description=Unicode single quotes
type=expr
~~~
# SOURCE
~~~roc
(
    'a',
    'é',
    '🚀',
    '\u(1F680)',
    '',
    'hello'
)
~~~
# EXPECTED
NIL
# PROBLEMS
**EMPTY CHARACTER LITERAL**
This character literal is empty.

**TOO LONG CHARACTER LITERAL**
This character literal is too long.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:6:5:6:7:**
```roc
    '',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'hello'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:7:5:7:12:**
```roc
    'hello'
```
    ^^^^^^^


**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it contains more than one character.
A single-quoted literal must contain exactly one character, e.g. 'a'.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenRound(1:1-1:2),
SingleQuote(2:5-2:8),Comma(2:8-2:9),
SingleQuote(3:5-3:9),Comma(3:9-3:10),
SingleQuote(4:5-4:11),Comma(4:11-4:12),
SingleQuote(5:5-5:16),Comma(5:16-5:17),
MalformedSingleQuote(6:5-6:7),Comma(6:7-6:8),
MalformedSingleQuote(7:5-7:12),
CloseRound(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-8.2
	(e-single-quote @2.5-2.8 (raw "'a'"))
	(e-single-quote @3.5-3.9 (raw "'é'"))
	(e-single-quote @4.5-4.11 (raw "'🚀'"))
	(e-single-quote @5.5-5.16 (raw "'\u(1F680)'"))
	(e-malformed @6.5-6.7 (reason "expr_unexpected_token"))
	(e-malformed @7.5-7.12 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
(
	'a',
	'é',
	'🚀',
	'\u(1F680)',
	,
	,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-8.2
	(elems
		(e-int @2.5-2.8 (value "97"))
		(e-int @3.5-3.9 (value "233"))
		(e-int @4.5-4.11 (value "128640"))
		(e-runtime-error (tag "too_long_single_quote"))
		(e-runtime-error (tag "tuple_elem_not_canonicalized"))
		(e-runtime-error (tag "tuple_elem_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "(Num(_size), Num(_size2), Num(_size3), Error, Error, Error)"))
~~~
