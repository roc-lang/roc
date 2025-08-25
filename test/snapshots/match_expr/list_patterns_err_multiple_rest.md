# META
~~~ini
description=Match expression with more than one rest pattern not permitted, should error
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot Comma LowerIdent Comma DoubleDot CloseSquare OpFatArrow TripleDot CloseCurly ~~~
# PARSE
~~~clojure
(match <5 branches>)
~~~
# FORMATTED
~~~roc
when numbers is { [<unary_double_dot>], middle, _ } -> <malformed> => ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:15

**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 2:5 to 2:10

**Parse Error**
at 1:15 to 2:20

**Parse Error**
at 1:1 to 2:20

**Parse Error**
at 2:20 to 2:20

**Unsupported Node**
at 1:15 to 2:20

**Unsupported Node**
at 2:20 to 2:28

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
