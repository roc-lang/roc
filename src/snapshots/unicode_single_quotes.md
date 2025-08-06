# META
~~~ini
description=Unicode single quotes
type=file
~~~
# SOURCE
~~~roc
module []

x = (
    'a',
    'é',
    '🚀',
    '\u',
    '\u)',
    '\u(',
    '\u()',
    '\u(1F680)',
    '\u(K)',
    '\\',
    '\'',
    '',
    'long',
    '\',
)

y = 'u

# Test backslash before EOF
'\
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID ESCAPE SEQUENCE - :0:0:0:0
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:7:5:7:9
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:8:5:8:10
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:9:5:9:10
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:10:5:10:11
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:12:5:12:12
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:15:5:15:7
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:16:5:16:11
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:17:5:17:9
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:20:5:20:7
PARSE ERROR - unicode_single_quotes.md:23:1:23:3
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
UNKNOWN OPERATOR - unicode_single_quotes.md:20:5:20:7
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:7:5:7:9:**
```roc
    '\u',
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u)'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:8:5:8:10:**
```roc
    '\u)',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u('** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:9:5:9:10:**
```roc
    '\u(',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u()'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:10:5:10:11:**
```roc
    '\u()',
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u(K)'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:12:5:12:12:**
```roc
    '\u(K)',
```
    ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:15:5:15:7:**
```roc
    '',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'long'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:16:5:16:11:**
```roc
    'long',
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:17:5:17:9:**
```roc
    '\',
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:20:5:20:7:**
```roc
y = 'u
```
    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**unicode_single_quotes.md:23:1:23:3:**
```roc
'\
```
^^


**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**INVALID TUPLE ELEMENT**
This tuple element is malformed or contains invalid syntax.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**unicode_single_quotes.md:20:5:20:7:**
```roc
y = 'u
```
    ^^

Check the spelling and make sure you're using a valid Roc operator like `+`, `-`, `==`.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),OpenRound(3:5-3:6),
SingleQuote(4:5-4:8),Comma(4:8-4:9),
SingleQuote(5:5-5:9),Comma(5:9-5:10),
SingleQuote(6:5-6:11),Comma(6:11-6:12),
MalformedSingleQuoteInvalidEscapeSequence(7:5-7:9),Comma(7:9-7:10),
MalformedSingleQuoteInvalidEscapeSequence(8:5-8:10),Comma(8:10-8:11),
MalformedSingleQuoteInvalidEscapeSequence(9:5-9:10),Comma(9:10-9:11),
MalformedSingleQuoteInvalidEscapeSequence(10:5-10:11),Comma(10:11-10:12),
SingleQuote(11:5-11:16),Comma(11:16-11:17),
MalformedSingleQuoteInvalidEscapeSequence(12:5-12:12),Comma(12:12-12:13),
SingleQuote(13:5-13:9),Comma(13:9-13:10),
SingleQuote(14:5-14:9),Comma(14:9-14:10),
MalformedSingleQuoteEmpty(15:5-15:7),Comma(15:7-15:8),
MalformedSingleQuoteTooLong(16:5-16:11),Comma(16:11-16:12),
MalformedSingleQuoteUnclosed(17:5-17:9),
CloseRound(18:1-18:2),
LowerIdent(20:1-20:2),OpAssign(20:3-20:4),MalformedSingleQuoteUnclosed(20:5-20:7),
MalformedSingleQuoteUnclosed(23:1-23:3),EndOfFile(23:3-23:3),
~~~
# PARSE
~~~clojure
(file @1.1-23.3
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-18.2
			(p-ident @3.1-3.2 (raw "x"))
			(e-tuple @3.5-18.2
				(e-single-quote @4.5-4.8 (raw "'a'"))
				(e-single-quote @5.5-5.9 (raw "'é'"))
				(e-single-quote @6.5-6.11 (raw "'🚀'"))
				(e-malformed @7.5-7.9 (reason "expr_unexpected_token"))
				(e-malformed @8.5-8.10 (reason "expr_unexpected_token"))
				(e-malformed @9.5-9.10 (reason "expr_unexpected_token"))
				(e-malformed @10.5-10.11 (reason "expr_unexpected_token"))
				(e-single-quote @11.5-11.16 (raw "'\u(1F680)'"))
				(e-malformed @12.5-12.12 (reason "expr_unexpected_token"))
				(e-single-quote @13.5-13.9 (raw "'\\'"))
				(e-single-quote @14.5-14.9 (raw "'\''"))
				(e-malformed @15.5-15.7 (reason "expr_unexpected_token"))
				(e-malformed @16.5-16.11 (reason "expr_unexpected_token"))
				(e-malformed @17.5-17.9 (reason "expr_unexpected_token"))))
		(s-decl @20.1-20.7
			(p-ident @20.1-20.2 (raw "y"))
			(e-malformed @20.5-20.7 (reason "expr_unexpected_token")))
		(s-malformed @23.1-23.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

x = (
	'a',
	'é',
	'🚀',
	,
	,
	,
	,
	'\u(1F680)',
	,
	'\\',
	'\'',
	,
	,
	,
)

y = 

# Test backslash before EOF

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-tuple @3.5-18.2
			(elems
				(e-int @4.5-4.8 (value "97"))
				(e-int @5.5-5.9 (value "233"))
				(e-int @6.5-6.11 (value "128640"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-int @11.5-11.16 (value "128640"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-int @13.5-13.9 (value "92"))
				(e-int @14.5-14.9 (value "39"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized")))))
	(d-let
		(p-assign @20.1-20.2 (ident "y"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "(Num(_size), Num(_size2), Num(_size3), Error, Error, Error, Error, Num(_size4), Error, Num(_size5), Num(_size6), Error, Error, Error)"))
		(patt @20.1-20.2 (type "Error")))
	(expressions
		(expr @3.5-18.2 (type "(Num(_size), Num(_size2), Num(_size3), Error, Error, Error, Error, Num(_size4), Error, Num(_size5), Num(_size6), Error, Error, Error)"))
		(expr @20.5-20.7 (type "Error"))))
~~~
