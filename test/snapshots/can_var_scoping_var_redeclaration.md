# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwVar LowerIdent OpAssign Int KwVar LowerIdent OpAssign Int LineComment LowerIdent OpAssign Int LineComment LowerIdent CloseCurly BlankLine LowerIdent OpAssign LowerIdent OpenRound OpenCurly CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "redeclareTest")
    (lambda
      (body
        (block
          (binop_equals
            (var_lc "x_")
            (num_literal_i32 5)
          )
          (binop_equals
            (var_lc "x_")
            (num_literal_i32 10)
          )
          (binop_equals
            (lc "x_")
            (num_literal_i32 15)
          )
          (lc "x_")
        )
      )
      (args
        (underscore)
      )
    )
  )
  (binop_equals
    (lc "result")
    (apply_lc
      (lc "redeclareTest")
      (record_literal)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10
	# Redeclare var - should warn but proceed
	x_ = 15
	# Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# EXPECTED
DUPLICATE DEFINITION - can_var_scoping_var_redeclaration.md:6:2:6:13
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "redeclareTest"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "result"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 26
(var #0 _)
(var #1 -> #23)
(var #2 _)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 Num *)
(var #8 _)
(var #9 _)
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 -> #23)
(var #15 _)
(var #16 -> #19)
(var #17 -> #25)
(var #18 -> #24)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 fn_pure)
(var #24 {})
(var #25 fn_pure)
~~~
# TYPES
~~~roc
redeclareTest : _arg -> _ret
result : _a
x_ : _a
~~~
