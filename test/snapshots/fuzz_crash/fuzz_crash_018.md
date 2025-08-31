# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# TOKENS
~~~text
Int LowerIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_colon
    (lc "b")
    (binop_pipe
      (uc "S")
      (uc "R")
    )
  )
)
~~~
# FORMATTED
~~~roc
0
b : S.R
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_018.md:1:1:1:2:**
```roc
0 b:S
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
