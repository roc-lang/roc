# META
~~~ini
description=Simple demonstration that type variable names avoid collision with existing identifiers
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
    result1 = identity(42)
    result2 = identity2("hello")
    result3 = pair(result1, result2)
    
    a + b + c
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "a")
    (num_literal_i32 1)
  )
  (binop_equals
    (lc "b")
    (num_literal_i32 2)
  )
  (binop_equals
    (lc "c")
    (num_literal_i32 3)
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
  (binop_equals
    (lc "identity2")
    (lambda
      (body
        (lc "y")
      )
      (args
        (lc "y")
      )
    )
  )
  (binop_equals
    (lc "pair")
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
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (block
      (binop_equals
        (lc "result1")
        (apply_lc
          (lc "identity")
          (num_literal_i32 42)
        )
      )
      (binop_equals
        (lc "result2")
        (apply_lc
          (lc "identity2")
          (str_literal_big "hello")
        )
      )
      (binop_equals
        (lc "result3")
        (apply_lc
          (lc "pair")
          (tuple_literal
            (lc "result1")
            (lc "result2")
          )
        )
      )
      (binop_plus
        (binop_plus
          (lc "a")
          (lc "b")
        )
        (lc "c")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - type_var_collision_simple.md:20:5:20:12
# PROBLEMS
**Parse Error**
at 17:7 to 17:7

**Unsupported Node**
at 9:12 to 9:16

**Unsupported Node**
at 12:13 to 12:17

**Unsupported Node**
at 15:8 to 15:24

**Unsupported Node**
at 17:5 to 17:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
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
