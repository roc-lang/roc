# META
~~~ini
description=Type annotation connection to definitions
type=file
~~~
# SOURCE
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent OpColon UpperIdent LowerIdent OpAssign LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "add_one")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "add_one")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "my_number")
    (uc "U64")
  )
  (binop_equals
    (lc "my_number")
    (apply_lc
      (lc "add_one")
      (num_literal_i32 42)
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	add_one,
	my_number,
]

add_one : U64 -> U64
add_one = \x -> x + 1

my_number : U64
my_number = add_one(42)
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
