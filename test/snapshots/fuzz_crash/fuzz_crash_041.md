# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpBar OpenRound Int Comma CloseRound OpOr OpBar Int ~~~
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
  (malformed)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_041.md:1:20:1:21
PARSE ERROR - fuzz_crash_041.md:1:21:1:22
PARSE ERROR - fuzz_crash_041.md:1:22:1:23
PARSE ERROR - fuzz_crash_041.md:1:23:1:24
PARSE ERROR - fuzz_crash_041.md:1:24:1:25
PARSE ERROR - fuzz_crash_041.md:1:25:1:26
PARSE ERROR - fuzz_crash_041.md:1:26:1:27
PARSE ERROR - fuzz_crash_041.md:1:27:1:28
PARSE ERROR - fuzz_crash_041.md:1:28:1:29
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
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
~~~
# TYPES
~~~roc
~~~
