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
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_012.md:1:3:1:17:**
```roc
||(|(l888888888|
```
  ^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
