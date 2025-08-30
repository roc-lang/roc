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
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

Pair((a, b)) : (a, b)
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |x, y| (y, x)
map_pair : Pair(a, b) -> (a -> c) -> (b -> d) -> Pair(c, d)
map_pair = |x, y, f, g| (f(x), g(y))
main = {
	p1 = swap_pair((1, 2))
	p2 = map_pair((3, 4, |x| x + 1, |y| y * 2))
	p2
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
