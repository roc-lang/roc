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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LineComment LineComment LineComment LowerIdent OpAssign OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma Int CloseRound CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma Int Comma OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound Comma OpenRound OpBar LowerIdent OpBar LowerIdent OpStar Int CloseRound CloseRound BlankLine LowerIdent CloseCurly ~~~
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


# Type alias with parameters, just like the original
Pair((a, b)) : (a, b)
# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |x, y| (y, x)
# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b) -> (a -> c) -> (b -> d) -> Pair(c, d)
map_pair = |x, y, f, g| (f(x), g(y))
# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
	# This creates Pair(Num, Num)
	p1 = swap_pair((1, 2))
	# This should fail - map_pair expects a tuple but gets four separate arguments
	# And the instantiated types from map_pair should cause issues
	p2 = map_pair((3, 4, |x| x + 1, |y| y * 2))
	p2
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **p1** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_p1` to suppress this warning.
The unused variable is declared here:

**test_exact_pattern_crash.md:19:5:19:7:**
```roc
    p1 = swap_pair((1, 2))
```
    ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
