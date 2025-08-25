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
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
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
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/main.roc" platform [main]) }

unused_regular = \x -> 42

# Underscore variable that is used - should warn
used_underscore = \_value -> _value

# Underscore variable that is unused - should be fine
unused_underscore = \_ignored -> 100

# Regular variable that is used - should be fine
used_regular = \number -> number + 1

main
(<malformed>! | _) | {
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
**Parse Error**
at 15:7 to 15:7

**Unsupported Node**
at 4:18 to 4:22

**Unsupported Node**
at 7:19 to 7:28

**Unsupported Node**
at 10:21 to 10:32

**Unsupported Node**
at 13:16 to 13:25

**Unsupported Node**
at 15:5 to 15:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "main")
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
