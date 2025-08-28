# META
~~~ini
description=Simple type alias usage in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

UserId : U64

getUser : UserId -> Str
getUser = |id| if (id > 10) "big" else "small"

main! = |_| getUser(100)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly UpperIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int CloseRound String KwElse String LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (lc "getUser")
    (binop_thin_arrow
      (uc "UserId")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getUser")
    (lambda
      (body
        (if_else <12 branches>)
      )
      (args
        (lc "id")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "getUser")
          (num_literal_i32 100)
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

UserId : U64
getUser : UserId -> Str
getUser = |id| if id > 10 "big" else "small"
main! = |_| getUser(100)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:16 to 6:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "getUser")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "getUser")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
getUser : _a
~~~
