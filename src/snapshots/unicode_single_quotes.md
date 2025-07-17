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
    '\t',
    '\u(1F680)',
    '\\',
    '\'',
    '',
    'hello'
)
~~~
# EXPECTED
EMPTY CHARACTER LITERAL - :0:0:0:0
TOO LONG CHARACTER LITERAL - :0:0:0:0
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:9:5:9:7
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:10:5:10:12
INVALID SCALAR - :0:0:0:0
INVALID SCALAR - :0:0:0:0
INVALID SCALAR - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
# PROBLEMS
**EMPTY CHARACTER LITERAL**
This character literal is empty.

**TOO LONG CHARACTER LITERAL**
This character literal is too long.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:9:5:9:7:**
```roc
    '',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'hello'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:10:5:10:12:**
```roc
    'hello'
```
    ^^^^^^^


**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

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
SingleQuote(5:5-5:9),Comma(5:9-5:10),
SingleQuote(6:5-6:16),Comma(6:16-6:17),
SingleQuote(7:5-7:9),Comma(7:9-7:10),
SingleQuote(8:5-8:9),Comma(8:9-8:10),
MalformedSingleQuote(9:5-9:7),Comma(9:7-9:8),
MalformedSingleQuote(10:5-10:12),
CloseRound(11:1-11:2),EndOfFile(11:2-11:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-11.2
	(e-single-quote @2.5-2.8 (raw "'a'"))
	(e-single-quote @3.5-3.9 (raw "'é'"))
	(e-single-quote @4.5-4.11 (raw "'🚀'"))
	(e-single-quote @5.5-5.9 (raw "'\t'"))
	(e-single-quote @6.5-6.16 (raw "'\u(1F680)'"))
	(e-single-quote @7.5-7.9 (raw "'\\'"))
	(e-single-quote @8.5-8.9 (raw "'\''"))
	(e-malformed @9.5-9.7 (reason "expr_unexpected_token"))
	(e-malformed @10.5-10.12 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
(
	'a',
	'é',
	'🚀',
	'\t',
	'\u(1F680)',
	'\\',
	'\'',
	,
	,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-11.2
	(elems
		(e-int @2.5-2.8 (value "97"))
		(e-int @3.5-3.9 (value "233"))
		(e-int @4.5-4.11 (value "128640"))
		(e-runtime-error (tag "invalid_single_quote"))
		(e-int @6.5-6.16 (value "128640"))
		(e-runtime-error (tag "invalid_single_quote"))
		(e-runtime-error (tag "invalid_single_quote"))
		(e-runtime-error (tag "tuple_elem_not_canonicalized"))
		(e-runtime-error (tag "tuple_elem_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(expr @1.1-11.2 (type "(Num(_size), Num(_size2), Num(_size3), Error, Num(_size4), Error, Error, Error, Error)"))
~~~
