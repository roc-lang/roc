# META
~~~ini
description=Unicode single quotes
type=snippet
~~~
# SOURCE
~~~roc
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
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:5:5:5:9
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:6:5:6:10
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:7:5:7:10
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:8:5:8:11
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:10:5:10:12
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:13:5:13:7
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:14:5:14:11
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:15:5:15:9
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:18:5:18:7
PARSE ERROR - unicode_single_quotes.md:21:1:21:3
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
UNRECOGNIZED SYNTAX - unicode_single_quotes.md:18:5:18:7
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
    '\u',
```
     ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
    '\u)',
```
     ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
    '\u(',
```
     ^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
    '\u()',
```
     ^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

```roc
    '\u(K)',
```
     ^^^^^


**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

```roc
'\

```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:5:5:5:9:**
```roc
    '\u',
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u)'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:5:6:10:**
```roc
    '\u)',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u('** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:7:5:7:10:**
```roc
    '\u(',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u()'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:8:5:8:11:**
```roc
    '\u()',
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u(K)'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:10:5:10:12:**
```roc
    '\u(K)',
```
    ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:13:5:13:7:**
```roc
    '',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'long'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:14:5:14:11:**
```roc
    'long',
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:15:5:15:9:**
```roc
    '\',
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:18:5:18:7:**
```roc
y = 'u
```
    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**unicode_single_quotes.md:21:1:21:3:**
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

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**unicode_single_quotes.md:18:5:18:7:**
```roc
y = 'u
```
    ^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),OpenRound(1:5-1:6),
SingleQuote(2:5-2:8),Comma(2:8-2:9),
SingleQuote(3:5-3:9),Comma(3:9-3:10),
SingleQuote(4:5-4:11),Comma(4:11-4:12),
MalformedSingleQuoteInvalidEscapeSequence(5:5-5:9),Comma(5:9-5:10),
MalformedSingleQuoteInvalidEscapeSequence(6:5-6:10),Comma(6:10-6:11),
MalformedSingleQuoteInvalidEscapeSequence(7:5-7:10),Comma(7:10-7:11),
MalformedSingleQuoteInvalidEscapeSequence(8:5-8:11),Comma(8:11-8:12),
SingleQuote(9:5-9:16),Comma(9:16-9:17),
MalformedSingleQuoteInvalidEscapeSequence(10:5-10:12),Comma(10:12-10:13),
SingleQuote(11:5-11:9),Comma(11:9-11:10),
SingleQuote(12:5-12:9),Comma(12:9-12:10),
MalformedSingleQuoteEmpty(13:5-13:7),Comma(13:7-13:8),
MalformedSingleQuoteTooLong(14:5-14:11),Comma(14:11-14:12),
MalformedSingleQuoteUnclosed(15:5-15:9),
CloseRound(16:1-16:2),
LowerIdent(18:1-18:2),OpAssign(18:3-18:4),MalformedSingleQuoteUnclosed(18:5-18:7),
MalformedSingleQuoteUnclosed(21:1-21:3),
EndOfFile(22:1-22:1),
~~~
# PARSE
~~~clojure
(file @1.1-21.3
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-16.2
			(p-ident @1.1-1.2 (raw "x"))
			(e-tuple @1.5-16.2
				(e-single-quote @2.5-2.8 (raw "'a'"))
				(e-single-quote @3.5-3.9 (raw "'é'"))
				(e-single-quote @4.5-4.11 (raw "'🚀'"))
				(e-malformed @5.5-5.9 (reason "expr_unexpected_token"))
				(e-malformed @6.5-6.10 (reason "expr_unexpected_token"))
				(e-malformed @7.5-7.10 (reason "expr_unexpected_token"))
				(e-malformed @8.5-8.11 (reason "expr_unexpected_token"))
				(e-single-quote @9.5-9.16 (raw "'\u(1F680)'"))
				(e-malformed @10.5-10.12 (reason "expr_unexpected_token"))
				(e-single-quote @11.5-11.9 (raw "'\\'"))
				(e-single-quote @12.5-12.9 (raw "'\''"))
				(e-malformed @13.5-13.7 (reason "expr_unexpected_token"))
				(e-malformed @14.5-14.11 (reason "expr_unexpected_token"))
				(e-malformed @15.5-15.9 (reason "expr_unexpected_token"))))
		(s-decl @18.1-18.7
			(p-ident @18.1-18.2 (raw "y"))
			(e-malformed @18.5-18.7 (reason "expr_unexpected_token")))
		(s-malformed @21.1-21.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @1.1-1.2 (ident "x"))
		(e-tuple @1.5-16.2
			(elems
				(e-num @2.5-2.8 (value "97"))
				(e-num @3.5-3.9 (value "233"))
				(e-num @4.5-4.11 (value "128640"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-num @9.5-9.16 (value "128640"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-num @11.5-11.9 (value "92"))
				(e-num @12.5-12.9 (value "39"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized")))))
	(d-let
		(p-assign @18.1-18.2 (ident "y"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "(Num(Int(_size)), Num(Int(_size2)), Num(Int(_size3)), Error, Error, Error, Error, Num(Int(_size4)), Error, Num(Int(_size5)), Num(Int(_size6)), Error, Error, Error)"))
		(patt @18.1-18.2 (type "Error")))
	(expressions
		(expr @1.5-16.2 (type "(Num(Int(_size)), Num(Int(_size2)), Num(Int(_size3)), Error, Error, Error, Error, Num(Int(_size4)), Error, Num(Int(_size5)), Num(Int(_size6)), Error, Error, Error)"))
		(expr @18.5-18.7 (type "Error"))))
~~~
