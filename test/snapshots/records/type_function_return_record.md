# META
~~~ini
description=Function type annotation returning record
type=statement
~~~
# SOURCE
~~~roc
create_user! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
# TOKENS
~~~text
LowerIdent OpBang OpColon UpperIdent Comma UpperIdent OpFatArrow OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(binop_colon
  (not_lc "create_user")
  (binop_thick_arrow
    (uc "Str")
    (binop_thick_arrow
      (uc "U32")
      (record_literal
        (binop_colon
          (lc "name")
          (uc "Str")
        )
        (binop_colon
          (lc "age")
          (uc "U32")
        )
        (binop_colon
          (lc "id")
          (uc "U64")
        )
        (binop_colon
          (lc "active")
          (uc "Bool")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
create_user! : Str => (U32 => { name : Str, age : U32, id : U64, active : Bool })
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:16 to 1:19

**Unsupported Node**
at 1:21 to 1:24

# CANONICALIZE
~~~clojure
(Stmt.type_anno)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
