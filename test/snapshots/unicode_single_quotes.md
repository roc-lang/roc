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
LowerIdent,OpAssign,OpenRound,
SingleQuote,Comma,
SingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuoteInvalidEscapeSequence,Comma,
MalformedSingleQuoteInvalidEscapeSequence,Comma,
MalformedSingleQuoteInvalidEscapeSequence,Comma,
MalformedSingleQuoteInvalidEscapeSequence,Comma,
SingleQuote,Comma,
MalformedSingleQuoteInvalidEscapeSequence,Comma,
SingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuoteEmpty,Comma,
MalformedSingleQuoteTooLong,Comma,
MalformedSingleQuoteUnclosed,
CloseRound,
LowerIdent,OpAssign,MalformedSingleQuoteUnclosed,
MalformedSingleQuoteUnclosed,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-tuple
				(e-single-quote (raw "'a'"))
				(e-single-quote (raw "'é'"))
				(e-single-quote (raw "'🚀'"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-single-quote (raw "'\u(1F680)'"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-single-quote (raw "'\\'"))
				(e-single-quote (raw "'\''"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))))
		(s-decl
			(p-ident (raw "y"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))))
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
		(p-assign (ident "x"))
		(e-tuple
			(elems
				(e-num (value "97"))
				(e-num (value "233"))
				(e-num (value "128640"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-num (value "128640"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-num (value "92"))
				(e-num (value "39"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized"))
				(e-runtime-error (tag "tuple_elem_not_canonicalized")))))
	(d-let
		(p-assign (ident "y"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Num(Int(_size)), Num(Int(_size2)), Num(Int(_size3)), Error, Error, Error, Error, Num(Int(_size4)), Error, Num(Int(_size5)), Num(Int(_size6)), Error, Error, Error)"))
		(patt (type "Error")))
	(expressions
		(expr (type "(Num(Int(_size)), Num(Int(_size2)), Num(Int(_size3)), Error, Error, Error, Error, Num(Int(_size4)), Error, Num(Int(_size5)), Num(Int(_size6)), Error, Error, Error)"))
		(expr (type "Error"))))
~~~
