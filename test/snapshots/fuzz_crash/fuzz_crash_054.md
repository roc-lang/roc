# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import S exposing[c as
f]
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent CloseSquare ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
(block
  (import
    (binop_exposing
      (uc "S")
      (list_literal
        (lc "c")
      )
    )
  )
  (malformed)
  (lc "f")
  (malformed)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

import S exposing [c]
as
f
]
~~~
# EXPECTED
MODULE NOT FOUND - fuzz_crash_054.md:1:20:2:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_054.md:1:40:2:1:**
```roc
app[]{f:platform""}import S exposing[c as
f]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_054.md:2:2:2:3:**
```roc
f]
```
 ^


**UNDEFINED VARIABLE**
Nothing is named **f** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_054.md:2:1:2:2:**
```roc
f]
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.malformed)
  (Expr.lookup "f")
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 17
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
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
~~~
# TYPES
~~~roc
~~~
