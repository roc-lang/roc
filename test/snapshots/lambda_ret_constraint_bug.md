# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform/main.roc" }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "helper")
    (binop_thin_arrow
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "helper")
    (lambda
      (body
        (binop_star
          (lc "n")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (binop_colon
    (lc "main")
    (binop_thin_arrow
      (uc "I64")
      (binop_thin_arrow
        (uc "I64")
        (uc "I64")
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (apply_lc
          (lc "helper")
          (num_literal_i32 5)
        )
      )
      (args
        (tuple_literal
          (underscore)
          (underscore)
        )
      )
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
at 3:10 to 3:20

**Unsupported Node**
at 4:10 to 4:14

**Unsupported Node**
at 6:8 to 6:23

**Unsupported Node**
at 7:8 to 7:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "helper")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "main")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
