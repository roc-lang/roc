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
|{ name : name, age : age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:2 to 1:15

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
