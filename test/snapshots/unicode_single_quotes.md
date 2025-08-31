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
    'Ã©',
    'ðŸš€',
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
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign OpenRound SingleQuote Comma MalformedSingleQuoteUnclosed MalformedUnknownToken MalformedSingleQuoteUnclosed MalformedSingleQuoteUnclosed MalformedUnknownToken MalformedUnknownToken MalformedUnknownToken MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence Comma MalformedSingleQuoteInvalidEscapeSequence CloseRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound CloseRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound Int UpperIdent CloseRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound UpperIdent CloseRound MalformedSingleQuoteUnclosed SingleQuote Comma SingleQuote Comma MalformedSingleQuoteEmpty Comma MalformedSingleQuoteUnclosed LowerIdent MalformedSingleQuoteUnclosed MalformedSingleQuoteUnclosed Comma CloseRound BlankLine LowerIdent OpAssign MalformedSingleQuoteUnclosed BlankLine LineComment MalformedSingleQuoteUnclosed ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x = ('a')
©
',
'ð
Ÿ
š
€
',
'\u'
,
'\u
)
',
'\u(',
)
'\u()
',
'\u(1F680)
F680
)
',
'\u(K)
',
'\\'
,
'\'
,
''
,
'l
ong
',
'\'
,
)

y = 'u

# Test backslash before EOF

'\
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **'Ã** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:5:5:5:7:**
```roc
    'Ã©',
```
    ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **©** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:5:7:5:8:**
```roc
    'Ã©',
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:5:8:6:5:**
```roc
    'Ã©',
    'ðŸš€',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'ð** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:5:6:7:**
```roc
    'ðŸš€',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ÿ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:7:6:8:**
```roc
    'ðŸš€',
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **š** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:8:6:9:**
```roc
    'ðŸš€',
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **€** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:9:6:10:**
```roc
    'ðŸš€',
```
        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:10:7:5:**
```roc
    'ðŸš€',
    '\u',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:7:5:7:9:**
```roc
    '\u',
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:7:9:8:5:**
```roc
    '\u',
    '\u)',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:8:5:8:8:**
```roc
    '\u)',
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:8:8:8:9:**
```roc
    '\u)',
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:8:9:9:5:**
```roc
    '\u)',
    '\u(',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:9:5:9:8:**
```roc
    '\u(',
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:9:9:10:5:**
```roc
    '\u(',
    '\u()',
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**unicode_single_quotes.md:9:5:10:5:**
```roc
    '\u(',
    '\u()',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:10:5:10:8:**
```roc
    '\u()',
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:10:10:11:5:**
```roc
    '\u()',
    '\u(1F680)',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:11:5:11:8:**
```roc
    '\u(1F680)',
```
    ^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**unicode_single_quotes.md:11:5:11:10:**
```roc
    '\u(1F680)',
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:11:14:11:15:**
```roc
    '\u(1F680)',
```
             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:11:15:12:5:**
```roc
    '\u(1F680)',
    '\u(K)',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:12:5:12:8:**
```roc
    '\u(K)',
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:12:11:13:5:**
```roc
    '\u(K)',
    '\\',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:13:9:14:5:**
```roc
    '\\',
    '\'',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:14:9:15:5:**
```roc
    '\'',
    '',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:15:5:15:7:**
```roc
    '',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:15:7:16:5:**
```roc
    '',
    'long',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'l** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:16:5:16:7:**
```roc
    'long',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:16:10:17:5:**
```roc
    'long',
    '\',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:17:5:17:8:**
```roc
    '\',
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:17:8:18:1:**
```roc
    '\',
)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:18:1:20:1:**
```roc
)

y = 'u
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'u

# Test backslash before EOF
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:20:5:23:1:**
```roc
y = 'u

# Test backslash before EOF
'\
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:23:1:23:3:**
```roc
'\
```
^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.tuple_literal
      (Expr.str_literal_small)
      (Expr.malformed)
    )
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "ong")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "y")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : _a
y : Error
~~~
