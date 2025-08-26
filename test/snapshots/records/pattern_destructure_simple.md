# META
~~~ini
description=Simple record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent CloseCurly OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <5 branches>)
~~~
# FORMATTED
~~~roc
when person is {
	{ name, age }
	=>
	name
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:19 to 2:19

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
