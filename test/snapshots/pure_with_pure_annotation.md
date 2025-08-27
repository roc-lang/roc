# META
~~~ini
description=Pure function with pure annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly Dot LowerIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
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
          (record_literal
            (binop_colon
              (lc "x")
              (lc "x")
            )
            (binop_colon
              (lc "y")
              (lc "y")
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
  (binop_equals
    (not_lc "main")
    (apply_lc
      (lc "add")
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

add : I32 -> I32 -> I32
add = \(x, y) -> { x : x, y : y } | .x

# Another pure function that calls a pure function
double : I32 -> I32
double = \x -> add((x, x))

main! = add((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
