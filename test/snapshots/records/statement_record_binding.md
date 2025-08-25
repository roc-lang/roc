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
  (block
    (binop_colon
      (lc "name")
      (binop_colon
        (tuple_literal
          (binop_colon
            (tuple_literal
              (str_literal_big "Alice")
              (lc "age")
            )
            (num_literal_i32 30)
          )
          (lc "email")
        )
        (str_literal_big "alice@example.com")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
person = {
	name: ((("Alice", age): 30, email): "alice@example.com")
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:41 to 1:42

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
