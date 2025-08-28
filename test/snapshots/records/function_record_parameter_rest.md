# META
~~~ini
description=Function with record parameter and rest pattern
type=expr
~~~
# SOURCE
~~~roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# TOKENS
~~~text
OpBar OpenCurly LowerIdent Comma DoubleDot LowerIdent CloseCurly OpBar String ~~~
# PARSE
~~~clojure
(lambda
  (body
    (str_literal_big "Hello ${first_name} ${rest.last_name}")
  )
  (args
    (record_literal
      (binop_colon
        (lc "first_name")
        (lc "first_name")
      )
      (double_dot_lc "rest")
    )
  )
)
~~~
# FORMATTED
~~~roc
|{ first_name : first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:2 to 1:24

# CANONICALIZE
~~~clojure
(Expr.lambda)
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
