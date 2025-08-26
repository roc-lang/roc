# META
~~~ini
description=Record in let binding statement
type=statement
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30, email: "alice@example.com" }
~~~
# TOKENS
~~~text
LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(binop_equals
  (lc "person")
  (record_literal
    (binop_colon
      (lc "name")
      (str_literal_big "Alice")
    )
    (binop_colon
      (lc "age")
      (num_literal_i32 30)
    )
    (binop_colon
      (lc "email")
      (str_literal_big "alice@example.com")
    )
  )
)
~~~
# FORMATTED
~~~roc
person = { name : "Alice", age : 30, email : "alice@example.com" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Stmt.assign)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
