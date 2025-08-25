# META
~~~ini
description=Let-polymorphism with empty list in expression
type=expr
~~~
# SOURCE
~~~roc
match [] {
    empty => { ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty }
}
~~~
# TOKENS
~~~text
KwMatch OpenSquare CloseSquare OpenCurly LowerIdent OpFatArrow OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(match <19 branches>)
~~~
# FORMATTED
~~~roc
when [] is {
	empty
	<malformed>
	{
		ints: ((([(1, 2, 3)], strs): [("a", "b", "c")], empty): empty)
	}
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:10

**Parse Error**
at 2:11 to 2:11

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 3:2 to 3:2

**Unsupported Node**
at 1:7 to 1:8

**Unsupported Node**
at 1:10 to 3:1

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
