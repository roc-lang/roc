# META
~~~ini
description=Nested if-then-else chain demonstrating flattening
type=file
~~~
# SOURCE
~~~roc
module [checkNumber]

checkNumber = |num| {
	if num < 0 {
		"negative"
	} else if num == 0 {
		"zero"
	} else if num > 100 {
		"large"
	} else {
		"positive"
	}
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwIf LowerIdent OpLessThan Int OpenCurly String CloseCurly KwElse KwIf LowerIdent OpEquals Int OpenCurly String CloseCurly KwElse KwIf LowerIdent OpGreaterThan Int OpenCurly String CloseCurly KwElse OpenCurly String CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "checkNumber")
))
(block
  (binop_equals
    (lc "checkNumber")
    (lambda
      (body
        (block
          (if_else
            (condition               (binop_lt
                (lc "num")
                (num_literal_i32 0)
              )
)
            (then               (block
                (str_literal_big "negative")
              )
)
            (else               (if_else
                (condition                   (binop_double_equals
                    (lc "num")
                    (num_literal_i32 0)
                  )
)
                (then                   (block
                    (str_literal_small "zero")
                  )
)
                (else                   (if_else
                    (condition                       (binop_gt
                        (lc "num")
                        (num_literal_i32 100)
                      )
)
                    (then                       (block
                        (str_literal_big "large")
                      )
)
                    (else                       (block
                        (str_literal_big "positive")
                      )
))
))
))
        )
      )
      (args
        (lc "num")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [checkNumber]

checkNumber = |num| {
	if num < 0
		{
			"negative"
		}
	else if num == 0
		{
			"zero"
		}
	else if num > 100
		{
			"large"
		}
	else {
		"positive"
	}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "checkNumber"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 34
(var #0 _)
(var #1 _)
(var #2 -> #33)
(var #3 _)
(var #4 -> #5)
(var #5 -> #6)
(var #6 -> #29)
(var #7 Str)
(var #8 -> #20)
(var #9 -> #10)
(var #10 Num *)
(var #11 -> #30)
(var #12 Str)
(var #13 -> #20)
(var #14 -> #15)
(var #15 -> #16)
(var #16 -> #32)
(var #17 Str)
(var #18 -> #20)
(var #19 Str)
(var #20 _)
(var #21 -> #20)
(var #22 -> #20)
(var #23 -> #20)
(var #24 _)
(var #25 -> #33)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 Num *)
(var #30 -> #31)
(var #31 _)
(var #32 Num *)
(var #33 fn_pure)
~~~
# TYPES
~~~roc
num : _a
~~~
