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
	<malformed>
	name
} -> <malformed>
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

**Unsupported Node**
at 1:14 to 3:1

**Unsupported Node**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
