# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app { |f: "c" platform [main!] }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# TOKENS
~~~text
KwApp OpenCurly OpBar LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly UpperIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int OpBang CloseRound String KwElse String OpUnaryMinus LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (list_literal
    (not_lc "main")
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (lc "ser")
    (binop_thin_arrow
      (uc "UserId")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getUser")
    (lambda
      (body
        (if_without_else <10 branches>)
      )
      (args
        (lc "id")
      )
    )
  )
  (str_literal_small "big")
  (malformed malformed:expr_unexpected_token)
  (binop_equals
    (binop_minus
      (str_literal_small "l")
      (not_lc "ain")
    )
    (lambda
      (body
        (apply_lc
          (lc "getUser")
          (num_literal_i32 900)
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
app {  }

platform[main!]
}

UserId : U64

ser : UserId -> Str
getUser = \id -> if id > 1 !)"big"
else

"l" - ain! = \_ -> getUser(900)
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Package or Platform Name**
at 1:1 to 1:7

**Expected Close Curly Brace**
at 1:1 to 1:7

**Parse Error**
at 1:15 to 1:15

**Parse Error**
at 1:32 to 1:32

**Parse Error**
at 6:26 to 6:26

**Parse Error**
at 6:16 to 6:26

**Parse Error**
at 6:27 to 6:27

**Parse Error**
at 6:35 to 6:35

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_or)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_not_equals)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
