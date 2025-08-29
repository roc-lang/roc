# META
~~~ini
description=Simple demonstration that type variable names avoid collision with existing identifiers
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
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
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

a = 1
b = 2
c = 3
identity = |x| x
identity2 = |y| y
pair = |first, second| (first, second)
main! = |_| {
	result1 = identity(42)
	result2 = identity2("hello")
	result3 = pair((result1, result2))
	(a + b) + c
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "a")
    (Expr.num_literal_i32 1)
  )
  (Expr.binop_equals
    (Expr.lookup "b")
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_equals
    (Expr.lookup "c")
    (Expr.num_literal_i32 3)
  )
  (Expr.binop_equals
    (Expr.lookup "identity")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "identity2")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "pair")
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
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
a : Num(_size)
b : Num(_size)
c : Num(_size)
identity : _d
identity2 : _d
pair : _d
~~~
