# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent LineComment KwVar LowerIdent OpAssign LowerIdent OpStar Int LineComment BlankLine LowerIdent OpAssign LowerIdent OpPlus LowerIdent LineComment LowerIdent OpPlus LowerIdent LineComment CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "testFunc")
    (lambda
      (body
        (block
          (binop_equals
            (lc "sum")
            (lc "input")
          )
          (binop_equals
            (var_lc "sum_")
            (binop_star
              (lc "input")
              (num_literal_i32 2)
            )
          )
          (binop_equals
            (lc "sum_")
            (binop_plus
              (lc "sum_")
              (lc "sum")
            )
          )
          (binop_plus
            (lc "sum")
            (lc "sum_")
          )
        )
      )
      (args
        (lc "input")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input
	# Regular identifier
	var sum_ = input * 2
	# Var with underscore - should not conflict
	sum_ = sum_ + sum
	# Reassign var - should work
	sum + sum_
}

# Both should be accessible
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**can_var_scoping_var_idents.md:4:1:4:9:**
```roc
testFunc = |input| {
```
^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "testFunc"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
(var #0 _)
(var #1 -> #24)
(var #2 _)
(var #3 -> #4)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #17)
(var #17 -> #18)
(var #18 _)
(var #19 _)
(var #20 -> #24)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
testFunc : _arg -> _ret
input : _a
~~~
