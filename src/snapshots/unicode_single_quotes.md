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
    'ñ',
    '🚀',
    '\u(1F680)',
    '\u(00E9)',
)
~~~ 
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **'a',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:2:5:2:9:**
```roc
    'a',
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'é',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:3:5:3:10:**
```roc
    'é',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'ñ',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:4:5:4:10:**
```roc
    'ñ',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'🚀',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:5:5:5:12:**
```roc
    '🚀',
```
    ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u(1F680)',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:6:5:6:17:**
```roc
    '\u(1F680)',
```
    ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u(00E9)',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:7:5:7:16:**
```roc
    '\u(00E9)',
```
    ^^^^^^^^^^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
SingleQuote(2:5-2:8),Comma(2:8-2:9),Newline(1:1-1:1),
SingleQuote(3:5-3:9),Comma(3:9-3:10),Newline(1:1-1:1),
SingleQuote(4:5-4:9),Comma(4:9-4:10),Newline(1:1-1:1),
SingleQuote(5:5-5:11),Comma(5:11-5:12),Newline(1:1-1:1),
SingleQuote(6:5-6:16),Comma(6:16-6:17),Newline(1:1-1:1),
SingleQuote(7:5-7:15),Comma(7:15-7:16),Newline(1:1-1:1),
CloseRound(8:1-8:2),Newline(1:1-1:1),
MalformedUnknownToken(9:1-9:2),MalformedUnknownToken(9:2-9:3),MalformedUnknownToken(9:3-9:4),EndOfFile(9:5-9:5),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-8.2
	(e-malformed @2.5-2.9 (reason "expr_unexpected_token"))
	(e-malformed @3.5-3.10 (reason "expr_unexpected_token"))
	(e-malformed @4.5-4.10 (reason "expr_unexpected_token"))
	(e-malformed @5.5-5.12 (reason "expr_unexpected_token"))
	(e-malformed @6.5-6.17 (reason "expr_unexpected_token"))
	(e-malformed @7.5-7.16 (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
(
	,
	,
	,
	,
	,
	,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-8.2 (id 73)
	(elems))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "()"))
~~~
