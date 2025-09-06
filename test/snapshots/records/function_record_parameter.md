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
      (lc "name")
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
|{ name, age: age }| "Hello ${name}, you are ${age.to_str()} years old"
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
name : _a
~~~
