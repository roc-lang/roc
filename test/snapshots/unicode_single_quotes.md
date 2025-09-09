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
    'ğŸš€',
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
(block
  (binop_equals
    (lc "x")
    (tuple_literal
      (str_literal_small "a")
      (malformed)
    )
  )
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (apply_anon
    (malformed)
    (malformed)
  )
  (apply_anon
    (malformed)
  )
  (malformed)
  (apply_anon
    (malformed)
    (num_literal_i32 1)
  )
  (uc "F680")
  (malformed)
  (malformed)
  (apply_anon
    (malformed)
    (uc "K")
  )
  (malformed)
  (str_literal_small "\")
  (malformed)
  (str_literal_small "'")
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (lc "ong")
  (malformed)
  (malformed)
  (malformed)
  (malformed)
  (binop_equals
    (lc "y")
    (malformed)
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

x = ('a')
©
',
'ğ
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
UNRECOGNIZED SYNTAX - unicode_single_quotes.md:20:5:20:7
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
    'ğŸš€',
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'ğ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:5:6:7:**
```roc
    'ğŸš€',
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ÿ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:7:6:8:**
```roc
    'ğŸš€',
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **š** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:8:6:9:**
```roc
    'ğŸš€',
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **€** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:9:6:10:**
```roc
    'ğŸš€',
```
        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **',
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unicode_single_quotes.md:6:10:7:5:**
```roc
    'ğŸš€',
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


**SHADOWING**
This definition shadows an existing one.

**unicode_single_quotes.md:3:1:3:2:**
```roc
x = (
```
^


**UNDEFINED VARIABLE**
Nothing is named **ong** in this scope.
Is there an **import** or **exposing** missing up-top?

**unicode_single_quotes.md:16:7:16:10:**
```roc
    'long',
```
      ^^^


**SHADOWING**
This definition shadows an existing one.

**unicode_single_quotes.md:20:1:20:2:**
```roc
y = 'u
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
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
  (Expr.fn_call)
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.fn_call)
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
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 89
(var #0 _)
(var #1 -> #52)
(var #2 Str)
(var #3 _)
(var #4 -> #52)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 Num *)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 Str)
(var #35 _)
(var #36 Str)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 -> #87)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 tuple)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 -> #67)
(var #66 _)
(var #67 fn_pure)
(var #68 -> #69)
(var #69 fn_pure)
(var #70 _)
(var #71 -> #72)
(var #72 fn_pure)
(var #73 _)
(var #74 _)
(var #75 -> #76)
(var #76 fn_pure)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
~~~
# TYPES
~~~roc
x : (Str, _field)
y : _a
~~~
