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
    (lc "main")
  )
  (unary_not <unary>)
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
  (binop_minus
    (str_literal_small "l")
    (lc "ain")
  )
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (apply_lc
      (lc "getUser")
      (num_literal_i32 900)
    )
  )
)
~~~
# FORMATTED
~~~roc
app {  }

<malformed>[main]<malformed>!
<malformed>
UserId: U64

ser: (UserId -> Str)
getUser = \id -> if id > 1 <malformed>!
"big"
<malformed>

"l" - ain
(<malformed>! | _) | getUser(900)
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
at 1:24 to 1:29

**Parse Error**
at 1:30 to 1:30

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

**Parse Error**
at 8:7 to 8:7

**Unsupported Node**
at 1:15 to 1:15

**Unsupported Node**
at 1:24 to 1:30

**Unsupported Node**
at 1:30 to 1:30

**Unsupported Node**
at 1:32 to 1:32

**Unsupported Node**
at 5:7 to 5:20

**Unsupported Node**
at 6:11 to 6:16

**Unsupported Node**
at 6:35 to 6:35

**Unsupported Node**
at 8:5 to 8:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.unary_not)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "ser")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.binop_minus
    (Expr.str_literal_small)
    (Expr.lookup "ain")
  )
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> _ret")
~~~
# TYPES
~~~roc
~~~
