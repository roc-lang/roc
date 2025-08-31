# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# TOKENS
~~~text
LowerIdent OpAssign String LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "me")
    (str_literal_small "luc")
  )
  (binop_equals
    (lc "foo")
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"hello ${namF** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_017.md:2:7:2:20:**
```roc
foo = "hello ${namF
```
      ^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "me"))
    (Expr.str_literal_small)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
