# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare KwIf UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan Int OpFatArrow String OpenSquare LowerIdent Comma LowerIdent CloseSquare KwIf LowerIdent OpEquals LowerIdent OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <27 branches>)
~~~
# FORMATTED
~~~roc
when value is {
	[first, ..as]
	rest
	]
	if List.len(rest) > 5 => "long list starting with ${Num.toStr first}" [x, y]
	if x == y => "pair of equal values: ${Num.toStr x}" _ => "other"
} -> 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 2:5 to 2:19

**Parse Error**
at 2:23 to 2:23

**Parse Error**
at 2:25 to 3:5

**Parse Error**
at 3:12 to 4:5

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:13 to 5:1

**Unsupported Node**
at 5:2 to 5:2

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
