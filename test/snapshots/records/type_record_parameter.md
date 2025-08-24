# META
~~~ini
description=Function type annotation with record parameter
type=statement
~~~
# SOURCE
~~~roc
process_things : { name : Str, age : U32, thing: a }, (a -> Str) -> Str
~~~
# TOKENS
~~~text
LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon LowerIdent CloseCurly Comma OpenRound LowerIdent OpArrow UpperIdent CloseRound OpArrow UpperIdent ~~~
# PARSE
~~~clojure
(binop_colon
  (lc "process_things")
  (binop_thin_arrow
    (block
      (binop_colon
        (lc "name")
        (binop_colon
          (tuple_literal
            (binop_colon
              (tuple_literal
                (uc "Str")
                (lc "age")
              )
              (uc "U32")
            )
            (lc "thing")
          )
          (lc "a")
        )
      )
    )
    (binop_thin_arrow
      (binop_thin_arrow
        (lc "a")
        (uc "Str")
      )
      (uc "Str")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:72

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
