# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <10 branches>)
~~~
# FORMATTED
~~~roc
when list is {
	[first]
	=>
	first
	[first, second]
	=>
	first + second
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:12

**Parse Error**
at 2:13 to 2:13

**Parse Error**
at 3:21 to 3:21

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

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
