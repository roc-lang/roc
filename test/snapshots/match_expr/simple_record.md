# META
~~~ini
description=Simple record destructuring in match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name } => name
    { age } => age
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent CloseCurly OpFatArrow LowerIdent OpenCurly LowerIdent CloseCurly OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (block
        (lc "name")
      )
      (lc "name")
    )
)
  (branch2     (binop_thick_arrow
      (block
        (lc "age")
      )
      (lc "age")
    )
))
~~~
# FORMATTED
~~~roc
match person
	{
		name
	} => name
	{
		age
	} => age
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:14 to 2:16

**Unsupported Node**
at 3:5 to 3:12

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
