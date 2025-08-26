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
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwIf LowerIdent OpLessThan Int OpenCurly String CloseCurly KwElse KwIf LowerIdent OpEquals Int OpenCurly String CloseCurly KwElse KwIf LowerIdent OpGreaterThan Int OpenCurly String CloseCurly KwElse OpenCurly String CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "checkNumber")
    (lambda
      (body
        (block
          (if_else <12 branches>)
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
module [
	checkNumber,
]

checkNumber = \num -> {
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
**Parse Error**
at 4:2 to 4:13

**Parse Error**
at 6:9 to 6:21

**Parse Error**
at 8:9 to 8:22

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
