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
    '\\',
    '\'',
    '',
    'long',
)
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:8:5:8:7
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:9:5:9:11
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:8:5:8:7:**
```roc
    '',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'long'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:9:5:9:11:**
```roc
    'long',
```
    ^^^^^^


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
SingleQuote(6:5-6:9),Comma(6:9-6:10),
SingleQuote(7:5-7:9),Comma(7:9-7:10),
MalformedSingleQuoteEmpty(8:5-8:7),Comma(8:7-8:8),
MalformedSingleQuoteTooLong(9:5-9:11),Comma(9:11-9:12),
CloseRound(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-10.2
	(e-single-quote @2.5-2.8 (raw "'a'"))
	(e-single-quote @3.5-3.9 (raw "'é'"))
	(e-single-quote @4.5-4.11 (raw "'🚀'"))
	(e-single-quote @5.5-5.16 (raw "'\u(1F680)'"))
	(e-single-quote @6.5-6.9 (raw "'\\'"))
	(e-single-quote @7.5-7.9 (raw "'\''"))
	(e-malformed @8.5-8.7 (reason "expr_unexpected_token"))
	(e-malformed @9.5-9.11 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
(
	'a',
	'é',
	'🚀',
	'\u(1F680)',
	'\\',
	'\'',
	,
	,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-10.2
	(elems
		(e-int @2.5-2.8 (value "97"))
		(e-int @3.5-3.9 (value "233"))
		(e-int @4.5-4.11 (value "128640"))
		(e-int @5.5-5.16 (value "128640"))
		(e-int @6.5-6.9 (value "92"))
		(e-int @7.5-7.9 (value "39"))
		(e-runtime-error (tag "tuple_elem_not_canonicalized"))
		(e-runtime-error (tag "tuple_elem_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(expr @1.1-10.2 (type "(Num(_size), Num(_size2), Num(_size3), Num(_size4), Num(_size5), Num(_size6), Error, Error)"))
~~~
