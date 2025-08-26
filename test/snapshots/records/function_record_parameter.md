# META
~~~ini
description=Function with record parameter destructuring and string interpolation
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# TOKENS
~~~text
OpBar OpenCurly LowerIdent Comma LowerIdent CloseCurly OpBar String ~~~
# PARSE
~~~clojure
(lambda
  (body
    (str_literal_big "Hello ${name}, you are ${age.to_str()} years old")
  )
  (args
    (record_literal
      (binop_colon
        (lc "name")
        (lc "name")
      )
      (binop_colon
        (lc "age")
        (lc "age")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
\{ name : name, age : age } -> "Hello ${name}, you are ${age.to_str()} years old"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
