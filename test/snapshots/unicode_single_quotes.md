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
    '\u(EDA0B5)'
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
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:5:6:5:8
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:6:6:6:8
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:7:6:7:9
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:8:6:8:10
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:10:6:10:16
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:11:6:11:11
SINGLE QUOTE EMPTY - unicode_single_quotes.md:14:5:14:7
SINGLE QUOTE TOO LONG - unicode_single_quotes.md:15:5:15:11
UNCLOSED SINGLE QUOTE - unicode_single_quotes.md:16:5:16:9
UNCLOSED SINGLE QUOTE - unicode_single_quotes.md:19:5:19:7
INVALID ESCAPE SEQUENCE - unicode_single_quotes.md:22:2:23:1
UNCLOSED SINGLE QUOTE - unicode_single_quotes.md:22:1:22:3
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:5:5:5:9
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:6:5:6:10
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:7:5:7:10
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:8:5:8:11
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:10:5:10:17
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:11:5:11:12
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:14:5:14:7
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:15:5:15:11
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:16:5:16:9
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:19:5:19:7
PARSE ERROR - unicode_single_quotes.md:22:1:22:3
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
UNRECOGNIZED SYNTAX - unicode_single_quotes.md:19:5:19:7
# PROBLEMS

┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  '\u',                                                                     │
 │   ‾‾                                                                       │
 └────────────────────────────────────────────── unicode_single_quotes.md:5:6 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  '\u)',                                                                    │
 │   ‾‾                                                                       │
 └────────────────────────────────────────────── unicode_single_quotes.md:6:6 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  '\u(',                                                                    │
 │   ‾‾‾                                                                      │
 └────────────────────────────────────────────── unicode_single_quotes.md:7:6 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  '\u()',                                                                   │
 │   ‾‾‾‾                                                                     │
 └────────────────────────────────────────────── unicode_single_quotes.md:8:6 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  '\u(EDA0B5)'                                                              │
 │   ‾‾‾‾‾‾‾‾‾‾                                                               │
 └───────────────────────────────────────────── unicode_single_quotes.md:10:6 ┘



┌─────────────────────────────────┐
│ INVALID UNICODE ESCAPE SEQUENCE ├─ This Unicode escape sequence is not ─────┐
└┬────────────────────────────────┘  valid.                                   │
 │                                                                            │
 │  '\u(K)',                                                                  │
 │   ‾‾‾‾‾                                                                    │
 └───────────────────────────────────────────── unicode_single_quotes.md:11:6 ┘



┌────────────────────┐
│ SINGLE QUOTE EMPTY ├─ Single-quoted literals must contain exactly one ──────┐
└┬───────────────────┘  valid UTF-8 codepoint.                                │
 │                                                                            │
 │  '',                                                                       │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────── unicode_single_quotes.md:14:5 ┘



┌───────────────────────┐
│ SINGLE QUOTE TOO LONG ├─ Single-quoted literals must contain exactly one ───┐
└┬──────────────────────┘  valid UTF-8 codepoint.                             │
 │                                                                            │
 │  'long',                                                                   │
 │  ‾‾‾‾‾‾                                                                    │
 └───────────────────────────────────────────── unicode_single_quotes.md:15:5 ┘



┌───────────────────────┐
│ UNCLOSED SINGLE QUOTE ├─ This single-quoted literal is missing a closing ───┐
└┬──────────────────────┘  quote.                                             │
 │                                                                            │
 │  '\',                                                                      │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────── unicode_single_quotes.md:16:5 ┘



┌───────────────────────┐
│ UNCLOSED SINGLE QUOTE ├─ This single-quoted literal is missing a closing ───┐
└┬──────────────────────┘  quote.                                             │
 │                                                                            │
 │  y = 'u                                                                    │
 │      ‾‾                                                                    │
 └───────────────────────────────────────────── unicode_single_quotes.md:19:5 ┘



┌─────────────────────────┐
│ INVALID ESCAPE SEQUENCE ├─ This escape sequence is not recognized. ─────────┐
└┬────────────────────────┘                                                   │
 │                                                                            │
 │  '\                                                                        │
 │                                                                            │
 │                                                                            │
 └───────────────────────────────────────────── unicode_single_quotes.md:22:2 ┘



┌───────────────────────┐
│ UNCLOSED SINGLE QUOTE ├─ This single-quoted literal is missing a closing ───┐
└┬──────────────────────┘  quote.                                             │
 │                                                                            │
 │  '\                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────── unicode_single_quotes.md:22:1 ┘



┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\u' is not expected in an ─────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '\u',                                                                     │
 │  ‾‾‾‾                                                                      │
 └────────────────────────────────────────────── unicode_single_quotes.md:5:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\u)' is not expected in an ────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '\u)',                                                                    │
 │  ‾‾‾‾‾                                                                     │
 └────────────────────────────────────────────── unicode_single_quotes.md:6:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\u(' is not expected in an ────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '\u(',                                                                    │
 │  ‾‾‾‾‾                                                                     │
 └────────────────────────────────────────────── unicode_single_quotes.md:7:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\u()' is not expected in an ───┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '\u()',                                                                   │
 │  ‾‾‾‾‾‾                                                                    │
 └────────────────────────────────────────────── unicode_single_quotes.md:8:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\u(EDA0B5)' is not expected ───┐
└┬───────────────────────────────┘  in an expression.                         │
 │                                                                            │
 │  '\u(EDA0B5)'                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └───────────────────────────────────────────── unicode_single_quotes.md:10:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\u(K)' is not expected in an ──┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '\u(K)',                                                                  │
 │  ‾‾‾‾‾‾‾                                                                   │
 └───────────────────────────────────────────── unicode_single_quotes.md:11:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '' is not expected in an ───────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '',                                                                       │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────── unicode_single_quotes.md:14:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token 'long' is not expected in an ───┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  'long',                                                                   │
 │  ‾‾‾‾‾‾                                                                    │
 └───────────────────────────────────────────── unicode_single_quotes.md:15:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token '\', is not expected in an ─────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  '\',                                                                      │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────── unicode_single_quotes.md:16:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token 'u is not expected in an ───────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  y = 'u                                                                    │
 │      ‾‾                                                                    │
 └───────────────────────────────────────────── unicode_single_quotes.md:19:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  '\                                                                        │
 │  ‾‾                                                                        │
 └───────────────────────────────────────────── unicode_single_quotes.md:22:1 ┘

    This is an unexpected parsing error. Please check your syntax.


INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



INVALID TUPLE ELEMENT

This tuple element is malformed or contains invalid syntax.



┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  y = 'u                                                                    │
 │      ‾‾                                                                    │
 └───────────────────────────────────────────── unicode_single_quotes.md:19:5 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenRound,
SingleQuote,Comma,
SingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuote,
MalformedSingleQuote,Comma,
SingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,
CloseRound,
LowerIdent,OpAssign,MalformedSingleQuote,
MalformedSingleQuote,
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
		(patt (type "(Dec, Dec, Dec, Error, Error, Error, Error, Dec, Error, Error, Dec, Dec, Error, Error, Error)"))
		(patt (type "Error")))
	(expressions
		(expr (type "(Dec, Dec, Dec, Error, Error, Error, Error, Dec, Error, Error, Dec, Dec, Error, Error, Error)"))
		(expr (type "Error"))))
~~~
