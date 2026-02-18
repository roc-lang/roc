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
PARSE ERROR - unicode_single_quotes.md:17:1:17:2
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:19:5:19:7
PARSE ERROR - unicode_single_quotes.md:22:1:22:3
UNRECOGNIZED SYNTAX - unicode_single_quotes.md:17:1:17:2
UNRECOGNIZED SYNTAX - unicode_single_quotes.md:19:5:19:7
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_single_quotes.md:5:6:5:8:**
```roc
    '\u',
```
     ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_single_quotes.md:6:6:6:8:**
```roc
    '\u)',
```
     ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_single_quotes.md:7:6:7:9:**
```roc
    '\u(',
```
     ^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_single_quotes.md:8:6:8:10:**
```roc
    '\u()',
```
     ^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_single_quotes.md:10:6:10:16:**
```roc
    '\u(EDA0B5)'
```
     ^^^^^^^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_single_quotes.md:11:6:11:11:**
```roc
    '\u(K)',
```
     ^^^^^


**SINGLE QUOTE EMPTY**
Single-quoted literals must contain exactly one valid UTF-8 codepoint.

**unicode_single_quotes.md:14:5:14:7:**
```roc
    '',
```
    ^^


**SINGLE QUOTE TOO LONG**
Single-quoted literals must contain exactly one valid UTF-8 codepoint.

**unicode_single_quotes.md:15:5:15:11:**
```roc
    'long',
```
    ^^^^^^


**UNCLOSED SINGLE QUOTE**
This single-quoted literal is missing a closing quote.

**unicode_single_quotes.md:16:5:16:9:**
```roc
    '\',
```
    ^^^^


**UNCLOSED SINGLE QUOTE**
This single-quoted literal is missing a closing quote.

**unicode_single_quotes.md:19:5:19:7:**
```roc
y = 'u
```
    ^^


**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

**unicode_single_quotes.md:22:2:23:1:**
```roc
'\

```


**UNCLOSED SINGLE QUOTE**
This single-quoted literal is missing a closing quote.

**unicode_single_quotes.md:22:1:22:3:**
```roc
'\
```
^^


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
The token **'\u(EDA0B5)'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:10:5:10:17:**
```roc
    '\u(EDA0B5)'
```
    ^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

**unicode_single_quotes.md:17:1:17:2:**
```roc
)
```
^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:19:5:19:7:**
```roc
y = 'u
```
    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**unicode_single_quotes.md:22:1:22:3:**
```roc
'\
```
^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**unicode_single_quotes.md:17:1:17:2:**
```roc
)
```
^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**unicode_single_quotes.md:19:5:19:7:**
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
			(e-malformed (reason "expected_expr_close_round_or_comma")))
		(s-decl
			(p-ident (raw "y"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
x = 
	

y = 

# Test backslash before EOF
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "y"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
