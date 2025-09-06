# META
~~~ini
description=Basic variable scoping behavior
type=file
~~~
# SOURCE
~~~roc
module []

# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
    x = 20  # Should shadow top-level x
    innerResult = {
        # Block scope
        z = x + y  # x should resolve to 20, y to 10
        z + 1
    }
    innerResult
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign Int LineComment LowerIdent OpAssign OpenCurly LineComment LowerIdent OpAssign LowerIdent OpPlus LowerIdent LineComment LowerIdent OpPlus Int CloseCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "y")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "outerFunc")
    (lambda
      (body
        (block
          (binop_equals
            (lc "x")
            (num_literal_i32 20)
          )
          (binop_equals
            (lc "innerResult")
            (block
              (binop_equals
                (lc "z")
                (binop_plus
                  (lc "x")
                  (lc "y")
                )
              )
              (binop_plus
                (lc "z")
                (num_literal_i32 1)
              )
            )
          )
          (binop_colon
            (lc "innerResult")
            (lc "innerResult")
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
module []

# Top-level variables
x = 5
y = 10
# Function that shadows outer variable
outerFunc = |_| {
	x = 20
	# Should shadow top-level x
	innerResult = {
		# Block scope
		z = x + y
		# x should resolve to 20, y to 10
		z + 1
	}

	innerResult : innerResult
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_basic_scoping.md:9:5:9:6
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 5)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.num_literal_i32 10)
  )
  (Stmt.assign
    (pattern (Patt.ident "outerFunc"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 31
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #5)
(var #5 Num *)
(var #6 _)
(var #7 -> #30)
(var #8 _)
(var #9 -> #10)
(var #10 Num *)
(var #11 _)
(var #12 -> #21)
(var #13 -> #16)
(var #14 -> #15)
(var #15 -> #16)
(var #16 _)
(var #17 _)
(var #18 -> #19)
(var #19 -> #20)
(var #20 Num *)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #30)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 fn_pure)
~~~
# TYPES
~~~roc
x : Num(_size)
y : Num(_size)
innerResult : _a
outerFunc : _arg -> _ret
z : _a
~~~
