# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow Int OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare UpperIdent Comma UpperIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <0 branches>)
~~~
# FORMATTED
~~~roc
match list
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:8 to 2:11

**Parse Error**
at 3:9 to 3:12

**Parse Error**
at 4:21 to 4:24

**Parse Error**
at 5:15 to 5:18

**Parse Error**
at 5:5 to 5:18

**Parse Error**
at 5:22 to 5:24

**Parse Error**
at 5:24 to 5:27

**Parse Error**
at 6:19 to 6:22

**Parse Error**
at 6:5 to 6:22

**Parse Error**
at 6:26 to 6:28

**Parse Error**
at 6:28 to 6:31

**Parse Error**
at 7:18 to 7:21

**Parse Error**
at 7:5 to 7:21

**Parse Error**
at 7:25 to 7:27

**Parse Error**
at 7:27 to 7:30

**Parse Error**
at 1:12 to 8:2

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
