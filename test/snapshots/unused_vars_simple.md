# META
~~~ini
description=Simple unused and used underscore variable test
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
    a = unused_regular(5)
    b = used_underscore(10)
    c = unused_underscore(15)
    d = used_regular(20)
    a + b + c + d
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "unused_regular")
    (lambda
      (body
        (num_literal_i32 42)
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "used_underscore")
    (lambda
      (body
        (lc "_value")
      )
      (args
        (lc "_value")
      )
    )
  )
  (binop_equals
    (lc "unused_underscore")
    (lambda
      (body
        (num_literal_i32 100)
      )
      (args
        (lc "_ignored")
      )
    )
  )
  (binop_equals
    (lc "used_regular")
    (lambda
      (body
        (binop_plus
          (lc "number")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "number")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "a")
            (apply_lc
              (lc "unused_regular")
              (num_literal_i32 5)
            )
          )
          (binop_equals
            (lc "b")
            (apply_lc
              (lc "used_underscore")
              (num_literal_i32 10)
            )
          )
          (binop_equals
            (lc "c")
            (apply_lc
              (lc "unused_underscore")
              (num_literal_i32 15)
            )
          )
          (binop_equals
            (lc "d")
            (apply_lc
              (lc "used_regular")
              (num_literal_i32 20)
            )
          )
          (binop_plus
            (binop_plus
              (binop_plus
                (lc "a")
                (lc "b")
              )
              (lc "c")
            )
            (lc "d")
          )
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
app
{
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

unused_regular = \x -> 42

# Underscore variable that is used - should warn
used_underscore = \_value -> _value

# Underscore variable that is unused - should be fine
unused_underscore = \_ignored -> 100

# Regular variable that is used - should be fine
used_regular = \number -> number + 1

main! = \_ -> {
	a = unused_regular(5)
	b = used_underscore(10)
	c = unused_underscore(15)
	d = used_regular(20)
	((a + b) + c) + d
}
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
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
~~~
