# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module [extract_age]

extract_age : { age : U64 } -> U64
extract_age = |person| {
    { age } = person

	{ a: 0 }.a + age - { a: 0 }.a
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenCurly LowerIdent CloseCurly OpAssign LowerIdent OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent OpPlus LowerIdent OpBinaryMinus OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "extract_age")
))
~~~
# FORMATTED
~~~roc
module [extract_age]

extract_age : {
	age : U64
} -> U64
extract_age = |person| {
	{
		age : age
	} = person
	(({
		a : 0
	} | .a) + age) - ({
		a : 0
	} | .a)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 7:2 to 7:10

**Unsupported Node**
at 7:21 to 7:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "extract_age")
    (Expr.binop_thin_arrow
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "age")
          (Expr.apply_tag)
        )
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "extract_age")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
extract_age : _b
~~~
