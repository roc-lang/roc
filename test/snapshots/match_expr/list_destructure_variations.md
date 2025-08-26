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
(match <41 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:12

**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:9 to 3:9

**Parse Error**
at 4:21 to 4:21

**Parse Error**
at 5:15 to 5:15

**Parse Error**
at 5:5 to 5:18

**Parse Error**
at 5:22 to 5:22

**Parse Error**
at 5:24 to 5:24

**Parse Error**
at 6:19 to 6:19

**Parse Error**
at 6:5 to 6:22

**Parse Error**
at 6:26 to 6:26

**Parse Error**
at 6:28 to 6:28

**Parse Error**
at 7:18 to 7:18

**Parse Error**
at 7:5 to 7:21

**Parse Error**
at 7:25 to 7:25

**Parse Error**
at 7:27 to 7:27

**Parse Error**
at 1:1 to 8:2

**Parse Error**
at 8:2 to 8:2

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
