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
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpenRound SingleQuote Comma MalformedSingleQuoteUnclosed MalformedUnknownToken MalformedSingleQuoteUnclosed MalformedSingleQuoteUnclosed MalformedUnknownToken MalformedUnknownToken MalformedUnknownToken MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence Comma MalformedSingleQuoteInvalidEscapeSequence CloseRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound CloseRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound Int UpperIdent CloseRound MalformedSingleQuoteUnclosed MalformedSingleQuoteInvalidEscapeSequence OpenRound UpperIdent CloseRound MalformedSingleQuoteUnclosed SingleQuote Comma SingleQuote Comma MalformedSingleQuoteEmpty Comma MalformedSingleQuoteUnclosed LowerIdent MalformedSingleQuoteUnclosed MalformedSingleQuoteUnclosed Comma CloseRound LowerIdent OpAssign MalformedSingleQuoteUnclosed MalformedSingleQuoteUnclosed ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (tuple_literal
      (str_literal_small "")
      (malformed malformed:expr_unexpected_token)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (malformed malformed:expr_unexpected_token)
    (malformed malformed:expr_unexpected_token)
  )
  (apply_anon
    (malformed malformed:expr_unexpected_token)
  )
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (malformed malformed:expr_unexpected_token)
    (num_literal_i32 1)
  )
  (uc "F680")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (apply_anon
    (malformed malformed:expr_unexpected_token)
    (uc "K")
  )
  (malformed malformed:expr_unexpected_token)
  (str_literal_small "")
  (malformed malformed:expr_unexpected_token)
  (str_literal_small "")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (lc "ong")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (binop_equals
    (lc "y")
    (malformed malformed:expr_unexpected_token)
  )
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
module []


x = ('a',
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
'\, <malformed>)
<malformed><malformed>
<malformed><malformed><malformed><malformed><malformed>
<malformed><malformed>
<malformed><malformed>
<malformed>
<malformed>(<malformed>)
<malformed>()
<malformed>
<malformed>(1F680)F680<malformed>
<malformed>
<malformed>(K)
<malformed>
'\\',
    '\'',
    '',
    'long',
    '\',
)

y = 'u

# Test backslash before EOF
'\
<malformed>
'\'',
    '',
    'long',
    '\',
)

y = 'u

# Test backslash before EOF
'\
<malformed>
<malformed><malformed>
<malformed>ong<malformed>
<malformed><malformed>
<malformed>
y = <malformed>

# Test backslash before EOF
<malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:5 to 5:5

**Parse Error**
at 5:7 to 5:7

**Parse Error**
at 5:7 to 5:7

**Parse Error**
at 5:8 to 5:8

**Parse Error**
at 6:5 to 6:5

**Parse Error**
at 6:7 to 6:7

**Parse Error**
at 6:8 to 6:8

**Parse Error**
at 6:9 to 6:9

**Parse Error**
at 6:10 to 6:10

**Parse Error**
at 7:5 to 7:5

**Parse Error**
at 7:9 to 7:9

**Parse Error**
at 8:5 to 8:5

**Parse Error**
at 8:8 to 8:8

**Parse Error**
at 8:9 to 8:9

**Parse Error**
at 9:5 to 9:5

**Parse Error**
at 9:9 to 9:9

**Parse Error**
at 9:5 to 10:5

**Parse Error**
at 10:5 to 10:5

**Parse Error**
at 10:10 to 10:10

**Parse Error**
at 11:5 to 11:5

**Parse Error**
at 11:5 to 11:10

**Parse Error**
at 11:14 to 11:14

**Parse Error**
at 11:15 to 11:15

**Parse Error**
at 12:5 to 12:5

**Parse Error**
at 12:11 to 12:11

**Parse Error**
at 13:9 to 13:9

**Parse Error**
at 14:9 to 14:9

**Parse Error**
at 15:5 to 15:5

**Parse Error**
at 15:7 to 15:7

**Parse Error**
at 16:5 to 16:5

**Parse Error**
at 16:10 to 16:10

**Parse Error**
at 17:5 to 17:5

**Parse Error**
at 17:8 to 17:8

**Parse Error**
at 18:1 to 18:1

**Parse Error**
at 20:5 to 20:5

**Parse Error**
at 23:1 to 23:1

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 5:7 to 5:7

**Unsupported Node**
at 5:8 to 5:8

**Unsupported Node**
at 6:5 to 6:5

**Unsupported Node**
at 6:7 to 6:7

**Unsupported Node**
at 6:8 to 6:8

**Unsupported Node**
at 6:9 to 6:9

**Unsupported Node**
at 6:10 to 6:10

**Unsupported Node**
at 7:5 to 7:5

**Unsupported Node**
at 7:9 to 7:9

**Unsupported Node**
at 8:5 to 8:5

**Unsupported Node**
at 8:8 to 8:8

**Unsupported Node**
at 8:9 to 8:9

**Unsupported Node**
at 9:5 to 9:5

**Unsupported Node**
at 9:9 to 9:9

**Unsupported Node**
at 10:5 to 10:5

**Unsupported Node**
at 10:10 to 10:10

**Unsupported Node**
at 11:5 to 11:5

**Unsupported Node**
at 11:14 to 11:14

**Unsupported Node**
at 11:15 to 11:15

**Unsupported Node**
at 12:5 to 12:5

**Unsupported Node**
at 12:11 to 12:11

**Unsupported Node**
at 13:9 to 13:9

**Unsupported Node**
at 14:9 to 14:9

**Unsupported Node**
at 15:5 to 15:5

**Unsupported Node**
at 15:7 to 15:7

**Unsupported Node**
at 16:5 to 16:5

**Unsupported Node**
at 16:10 to 16:10

**Unsupported Node**
at 17:5 to 17:5

**Unsupported Node**
at 17:8 to 17:8

**Unsupported Node**
at 18:1 to 18:1

**Unsupported Node**
at 20:5 to 20:5

**Unsupported Node**
at 23:1 to 23:1

# CANONICALIZE
~~~clojure
(Expr.block
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
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
