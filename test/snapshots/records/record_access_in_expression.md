# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(binop_plus
  (binop_pipe
    (lc "person")
    (dot_lc "age")
  )
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
person.age + 5
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_access_in_expression.md:1:1:1:11:**
```roc
person.age + 5
```
^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.binop_plus
  (Expr.malformed)
  (Expr.num_literal_i32 5)
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_plus :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
