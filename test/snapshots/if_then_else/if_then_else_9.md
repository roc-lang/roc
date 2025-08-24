# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse KwIf Int OpenCurly UpperIdent CloseCurly KwElse OpenCurly Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else <3 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_9.md:1:4:1:8
INVALID IF CONDITION - if_then_else_9.md:3:11:3:11
INCOMPATIBLE IF BRANCHES - if_then_else_9.md:1:1:1:1
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 3:8 to 3:14

# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
