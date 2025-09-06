# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# TOKENS
~~~text
OpOr OpenRound OpBar OpenRound LowerIdent OpBar ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (malformed)
    (lambda
      (body
        (malformed)
      )
      (args
        (lc "l888888888")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
(|l888888888| )
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_012.md:1:1:1:2
PARSE ERROR - fuzz_crash_012.md:1:2:1:3
PARSE ERROR - fuzz_crash_012.md:1:3:1:4
PARSE ERROR - fuzz_crash_012.md:1:4:1:5
PARSE ERROR - fuzz_crash_012.md:1:5:1:6
PARSE ERROR - fuzz_crash_012.md:1:6:1:16
PARSE ERROR - fuzz_crash_012.md:1:16:1:17
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:3:1:17:**
```roc
||(|(l888888888|
```
  ^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #10)
(var #5 _)
(var #6 _)
(var #7 -> #11)
(var #8 _)
(var #9 _)
(var #10 fn_pure)
(var #11 fn_pure)
~~~
# TYPES
~~~roc
l888888888 : _a
~~~
