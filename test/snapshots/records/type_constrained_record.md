# META
~~~ini
description=Constrained record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, ..a } => Str
~~~
# TOKENS
~~~text
LowerIdent OpBang OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma DoubleDot LowerIdent CloseCurly OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(binop_colon
  (not_lc "process_user")
  (binop_thick_arrow
    (record_literal
      (binop_colon
        (lc "name")
        (uc "Str")
      )
      (binop_colon
        (lc "age")
        (uc "U32")
      )
      (double_dot_lc "a")
    )
    (uc "Str")
  )
)
~~~
# FORMATTED
~~~roc
process_user!: ({ name: Str, age: U32, ..a } => Str)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:54

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
