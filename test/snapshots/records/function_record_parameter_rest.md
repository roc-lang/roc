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
|{ first_name: first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lambda (canonicalized))
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 Str)
(var #6 -> #8)
(var #7 _)
(var #8 fn_pure)
~~~
# TYPES
~~~roc
rest : _a
~~~
