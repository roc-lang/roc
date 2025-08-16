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
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:9:5:9:11
PARSE ERROR - unicode_single_quotes.md:18:1:18:2
UNEXPECTED TOKEN IN EXPRESSION - unicode_single_quotes.md:20:5:20:7
PARSE ERROR - unicode_single_quotes.md:23:1:23:3
UNKNOWN OPERATOR - unicode_single_quotes.md:18:1:18:2
UNKNOWN OPERATOR - unicode_single_quotes.md:20:5:20:7
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.```roc
    '\u',
```
     ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.```roc
    '\u)',
```
     ^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.```roc
    '\u(',
```
     ^^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.```roc
    '\u()',
```
     ^^^^


**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.```roc
    '\u(K)',
```
     ^^^^^


**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.```roc
'\
```
 ^


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
The token **'\u(',** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unicode_single_quotes.md:9:5:9:11:**
```roc
    '\u(',
```
    ^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**unicode_single_quotes.md:18:1:18:2:**
```roc
)
```
^


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


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**unicode_single_quotes.md:18:1:18:2:**
```roc
)
```
^

Check the spelling and make sure you're using a valid Roc operator like `+`, `-`, `==`.

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
MalformedSingleQuoteUnclosed(9:5-9:11),
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
			(e-malformed @18.1-18.2 (reason "expected_expr_close_round_or_comma")))
		(s-decl @20.1-20.7
			(p-ident @20.1-20.2 (raw "y"))
			(e-malformed @20.5-20.7 (reason "expr_unexpected_token")))
		(s-malformed @23.1-23.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

x = 
	

y = 

# Test backslash before EOF

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign @20.1-20.2 (ident "y"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Error"))
		(patt @20.1-20.2 (type "Error")))
	(expressions
		(expr @18.1-18.2 (type "Error"))
		(expr @20.5-20.7 (type "Error"))))
~~~
