# META
~~~ini
description=Polymorphic tuple function with instantiation crash
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
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


# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |x, y| (y, x)
# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap((1, 2))
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
