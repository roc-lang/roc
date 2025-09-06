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
|{ name: name, age: age }| "Hello ${name}, you are ${age.to_str()} years old"
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
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 Str)
(var #7 -> #9)
(var #8 _)
(var #9 fn_pure)
~~~
# TYPES
~~~roc
age : _a
name : _a
~~~
