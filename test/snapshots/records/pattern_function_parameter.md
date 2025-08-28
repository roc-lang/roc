# META
~~~ini
description=Record pattern destructuring in function parameter
type=statement
~~~
# SOURCE
~~~roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
~~~
# TOKENS
~~~text
LowerIdent OpAssign OpBar OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly OpBar String ~~~
# PARSE
~~~clojure
(binop_equals
  (lc "formatUser")
  (lambda
    (body
      (str_literal_big "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}")
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
        (binop_colon
          (lc "email")
          (lc "email")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
formatUser = |{ name : name, age : age, email : email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
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
