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
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:5:5:5:9
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:6:5:6:10
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:7:5:7:10
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:8:5:8:11
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:10:5:10:17
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:11:5:11:12
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:14:5:14:7
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:15:5:15:11
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:16:5:16:9
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:19:5:19:7
UNEXPECTED STATEMENT - unicode_single_quotes.md:22:1:22:3
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
── ✗ invalid unicode escape sequence ────────────── unicode_single_quotes.md:5:6

This Unicode escape sequence is not valid.

'\u',
 ^^

── ✗ invalid unicode escape sequence ────────────── unicode_single_quotes.md:6:6

This Unicode escape sequence is not valid.

'\u)',
 ^^

── ✗ invalid unicode escape sequence ────────────── unicode_single_quotes.md:7:6

This Unicode escape sequence is not valid.

'\u(',
 ^^^

── ✗ invalid unicode escape sequence ────────────── unicode_single_quotes.md:8:6

This Unicode escape sequence is not valid.

'\u()',
 ^^^^

── ✗ invalid unicode escape sequence ───────────── unicode_single_quotes.md:10:6

This Unicode escape sequence is not valid.

'\u(EDA0B5)'
 ^^^^^^^^^^

── ✗ invalid unicode escape sequence ───────────── unicode_single_quotes.md:11:6

This Unicode escape sequence is not valid.

'\u(K)',
 ^^^^^

── ✗ single quote empty ────────────────────────── unicode_single_quotes.md:14:5

Single-quoted literals must contain exactly one valid UTF-8 codepoint.

'',
^^

── ✗ single quote too long ─────────────────────── unicode_single_quotes.md:15:5

Single-quoted literals must contain exactly one valid UTF-8 codepoint.

'long',
^^^^^^

── ✗ unclosed single quote ─────────────────────── unicode_single_quotes.md:16:5

This single-quoted literal is missing a closing quote.

'\',
^^^^

── ✗ unclosed single quote ─────────────────────── unicode_single_quotes.md:19:5

This single-quoted literal is missing a closing quote.

y = 'u
    ^^

── ✗ invalid escape sequence ───────────────────── unicode_single_quotes.md:22:2

This escape sequence is not recognized.

'\


── ✗ unclosed single quote ─────────────────────── unicode_single_quotes.md:22:1

This single-quoted literal is missing a closing quote.

'\
^^

── ✗ unexpected expression syntax ───────────────── unicode_single_quotes.md:5:5

I was parsing an expression, and this token cannot start an expression here.

'\u',
^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\u' here.

── ✗ unexpected expression syntax ───────────────── unicode_single_quotes.md:6:5

I was parsing an expression, and this token cannot start an expression here.

'\u)',
^^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\u)' here.

── ✗ unexpected expression syntax ───────────────── unicode_single_quotes.md:7:5

I was parsing an expression, and this token cannot start an expression here.

'\u(',
^^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\u(' here.

── ✗ unexpected expression syntax ───────────────── unicode_single_quotes.md:8:5

I was parsing an expression, and this token cannot start an expression here.

'\u()',
^^^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\u()' here.

── ✗ unexpected expression syntax ──────────────── unicode_single_quotes.md:10:5

I was parsing an expression, and this token cannot start an expression here.

'\u(EDA0B5)'
^^^^^^^^^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\u(EDA0B5)' here.

── ✗ unexpected expression syntax ──────────────── unicode_single_quotes.md:11:5

I was parsing an expression, and this token cannot start an expression here.

'\u(K)',
^^^^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\u(K)' here.

── ✗ unexpected expression syntax ──────────────── unicode_single_quotes.md:14:5

I was parsing an expression, and this token cannot start an expression here.

'',
^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '' here.

── ✗ unexpected expression syntax ──────────────── unicode_single_quotes.md:15:5

I was parsing an expression, and this token cannot start an expression here.

'long',
^^^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found 'long' here.

── ✗ unexpected expression syntax ──────────────── unicode_single_quotes.md:16:5

I was parsing an expression, and this token cannot start an expression here.

'\',
^^^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found '\', here.

── ✗ unexpected expression syntax ──────────────── unicode_single_quotes.md:19:5

I was parsing an expression, and this token cannot start an expression here.

y = 'u
    ^^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found 'u here.

── ✗ unexpected statement ──────────────────────── unicode_single_quotes.md:22:1

I was parsing a statement, and this token cannot start a statement here.

'\
^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found '\ here.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ invalid tuple element ─────────────────────────────────────────────────────

This tuple element is malformed or contains invalid syntax.

── ✗ unrecognized syntax ───────────────────────── unicode_single_quotes.md:19:5

I don't recognize this syntax.

y = 'u
    ^^

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
	(type-mod)
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
x = ('a', 'é', '🚀', , , , , '\u(1F680)', , , '\\', '\'', , , )

y =

# Test backslash before EOF
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "erroneous_value_expr")))
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
