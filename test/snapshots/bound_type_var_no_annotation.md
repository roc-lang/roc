# META
~~~ini
description=A bound type variable (for identity function) with no type annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "combine")
    (binop_thin_arrow
      (lc "a")
      (binop_thin_arrow
        (lc "b")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
    )
  )
  (binop_equals
    (lc "combine")
    (lambda
      (body
        (tuple_literal
          (lc "first")
          (lc "second")
        )
      )
      (args
        (tuple_literal
          (lc "first")
          (lc "second")
        )
      )
    )
  )
  (binop_colon
    (lc "addOne")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "addOne")
    (lambda
      (body
        (binop_plus
          (lc "n")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "n")
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
        (lc "num")
        (apply_lc
          (lc "identity")
          (num_literal_i32 42)
        )
      )
      (binop_equals
        (lc "text")
        (apply_lc
          (lc "identity")
          (str_literal_big "hello")
        )
      )
      (binop_equals
        (lc "pair")
        (apply_lc
          (lc "combine")
          (tuple_literal
            (lc "num")
            (lc "text")
          )
        )
      )
      (binop_equals
        (lc "result")
        (apply_lc
          (lc "addOne")
          (num_literal_i32 5)
        )
      )
      (lc "result")
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/main.roc" platform [main]) }

identity = \x -> x

# Test function with multiple type parameters
combine: (a -> (b -> (a, b)))
combine = \(first, second) -> (first, second)
addOne: (U64 -> U64)
addOne = \n -> n + 1
main
(<malformed>! | _) | {
	num = identity(42)
	text = identity("hello")
	pair = combine((num, text))
	result = addOne(5)
	result
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 13:7 to 13:7

**Unsupported Node**
at 3:12 to 3:16

**Unsupported Node**
at 6:11 to 6:25

**Unsupported Node**
at 7:11 to 7:27

**Unsupported Node**
at 10:10 to 10:20

**Unsupported Node**
at 11:10 to 11:14

**Unsupported Node**
at 13:5 to 13:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "combine")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "addOne")
    (Expr.malformed)
  )
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
