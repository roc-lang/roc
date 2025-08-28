# META
~~~ini
description=Exact pattern from type_alias_parameterized with variations
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

# Type alias with parameters, just like the original
Pair(a, b) : (a, b)

# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |(x, y)| (y, x)

# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b), (a -> c), (b -> d) -> Pair(c, d)
map_pair = |(x, y), f, g| (f(x), g(y))

# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
    # This creates Pair(Num, Num)
    p1 = swap_pair((1, 2))

    # This should fail - map_pair expects a tuple but gets four separate arguments
    # And the instantiated types from map_pair should cause issues
    p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))

    p2
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpAssign OpenCurly LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma Int CloseRound CloseRound LowerIdent OpAssign LowerIdent OpenRound Int Comma Int Comma OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound Comma OpenRound OpBar LowerIdent OpBar LowerIdent OpStar Int CloseRound CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (tuple_literal
      (lc "a")
      (lc "b")
    )
  )
  (binop_colon
    (lc "swap_pair")
    (binop_thin_arrow
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "b")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "swap_pair")
    (lambda
      (body
        (tuple_literal
          (lc "y")
          (lc "x")
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
    (lc "map_pair")
    (binop_thin_arrow
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "a")
          (lc "c")
        )
        (binop_thin_arrow
          (binop_thin_arrow
            (lc "b")
            (lc "d")
          )
          (apply_uc
            (uc "Pair")
            (tuple_literal
              (lc "c")
              (lc "d")
            )
          )
        )
      )
    )
  )
  (binop_equals
    (lc "map_pair")
    (lambda
      (body
        (tuple_literal
          (apply_lc
            (lc "f")
            (lc "x")
          )
          (apply_lc
            (lc "g")
            (lc "y")
          )
        )
      )
      (args
        (tuple_literal
          (tuple_literal
            (lc "x")
            (lc "y")
          )
          (lc "f")
          (lc "g")
        )
      )
    )
  )
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "p1")
        (apply_lc
          (lc "swap_pair")
          (tuple_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
          )
        )
      )
      (binop_equals
        (lc "p2")
        (apply_lc
          (lc "map_pair")
          (tuple_literal
            (num_literal_i32 3)
            (num_literal_i32 4)
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
            (lambda
              (body
                (binop_star
                  (lc "y")
                  (num_literal_i32 2)
                )
              )
              (args
                (lc "y")
              )
            )
          )
        )
      )
      (lc "p2")
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

Pair((a, b)) : (a, b)
swap_pair : Pair (a, b) -> Pair (b, a)
swap_pair = |x, y| (y, x)
map_pair : Pair (a, b) -> (a -> c) -> (b -> d) -> Pair (c, d)
map_pair = |(x, y), f, g| (f(x), g(y))
main = {
		# This creates Pair(Num, Num)
p1 = swap_pair((1, 2))
	p2 = map_pair((3, 4, |x| x + 1, |y| y * 2))
	p2
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 8:15 to 8:19

**Unsupported Node**
at 12:14 to 12:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.lookup "a")
      (Expr.lookup "b")
    )
  )
  (Expr.binop_colon
    (Expr.lookup "swap_pair")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "swap_pair")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "map_pair")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.lookup "c")
        )
        (Expr.binop_thin_arrow
          (Expr.binop_thin_arrow
            (Expr.lookup "b")
            (Expr.lookup "d")
          )
          (Expr.apply_tag)
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "map_pair")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.block
      (Expr.binop_equals
        (Expr.lookup "p1")
        (Expr.apply_ident)
      )
      (Expr.binop_equals
        (Expr.lookup "p2")
        (Expr.apply_ident)
      )
      (Expr.lookup "p2")
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
swap_pair : _e
map_pair : _e
main : _e
~~~
