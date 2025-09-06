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
; Total type variables: 30
(var #0 _)
(var #1 _)
(var #2 -> #29)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 Str)
(var #8 _)
(var #9 _)
(var #10 Num *)
(var #11 _)
(var #12 Str)
(var #13 _)
(var #14 _)
(var #15 Num *)
(var #16 _)
(var #17 Str)
(var #18 _)
(var #19 Str)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #29)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 fn_pure)
~~~
# TYPES
~~~roc
checkNumber : _arg -> _ret
num : _a
~~~
