# META
~~~ini
description=Record with field update using old syntax (should give nice error message)
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAmpersand LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "person")
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
)
~~~
# FORMATTED
~~~roc
person
& 
age : 31
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_field_update_error.md:1:10:1:12:**
```roc
{ person & age: 31 }
```
         ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "person")
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 31)
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
