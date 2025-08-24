# META
~~~ini
description=Basic type annotations with type variables and application
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
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
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - type_annotation_basic.md:21:5:21:9
# PROBLEMS
**Parse Error**
at 15:7 to 15:7

**Unsupported Node**
at 4:12 to 4:18

**Unsupported Node**
at 5:12 to 5:16

**Unsupported Node**
at 8:11 to 8:25

**Unsupported Node**
at 9:11 to 9:27

**Unsupported Node**
at 12:10 to 12:20

**Unsupported Node**
at 13:10 to 13:14

**Unsupported Node**
at 15:5 to 15:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.malformed)
  )
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
