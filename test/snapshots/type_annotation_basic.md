# META
~~~ini
description=Basic type annotations with type variables and application
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Test generic identity function
identity : a -> a
identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
    # Test identity with different types
    num = identity(42)
    text = identity("hello")

    # Test combine function
    pair = combine(num, text)

    # Test concrete function
    result = addOne(5)

    result
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

identity : a -> a
identity = |x| x
combine : a -> b -> (a, b)
combine = |first, second| (first, second)
addOne : U64 -> U64
addOne = |n| n + 1
main! = |_| {
	num = identity(42)
	text = identity("hello")
	pair = combine((num, text))
	result = addOne(5)
	result : result
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
    (Expr.lookup "identity")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.lookup "a")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "identity")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "combine")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "b")
        (Expr.tuple_literal
          (Expr.lookup "a")
          (Expr.lookup "b")
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "combine")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "addOne")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "addOne")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
identity : _c
combine : _c
addOne : _c
~~~
