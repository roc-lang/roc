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
