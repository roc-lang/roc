# META
~~~ini
description=Pure function with pure annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Function with pure annotation using thin arrow
add : I32, I32 -> I32
add = |x, y| { x: x, y: y }.x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add(x, x)

main! = add(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly Dot LowerIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "add")
    (binop_thin_arrow
      (uc "I32")
      (binop_thin_arrow
        (uc "I32")
        (uc "I32")
      )
    )
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (binop_pipe
          (block
            (binop_colon
              (lc "x")
              (binop_colon
                (tuple_literal
                  (lc "x")
                  (lc "y")
                )
                (lc "y")
              )
            )
          )
          (dot_lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
        )
      )
    )
  )
  (binop_colon
    (lc "double")
    (binop_thin_arrow
      (uc "I32")
      (uc "I32")
    )
  )
  (binop_equals
    (lc "double")
    (lambda
      (body
        (apply_lc
          (lc "add")
          (tuple_literal
            (lc "x")
            (lc "x")
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (lc "main")
  (unary_not <unary>)
  (apply_lc
    (lc "add")
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
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
**Parse Error**
at 11:7 to 11:7

**Unsupported Node**
at 4:7 to 4:22

**Unsupported Node**
at 5:7 to 5:14

**Unsupported Node**
at 8:10 to 8:20

**Unsupported Node**
at 9:10 to 9:14

**Unsupported Node**
at 11:7 to 11:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "add")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "double")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.malformed)
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
