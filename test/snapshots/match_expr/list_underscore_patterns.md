# META
~~~ini
description=Match expression with underscore patterns in list matching
type=expr
~~~
# SOURCE
~~~roc
match items {
    [_] => 1 # pattern match on a list with a single (ignored) element
    [.., last] => last # pattern match on the last item in the list
    [first, ..] => first # pattern match on the first item in the list
    [_, _, third] => third # pattern match on the third item in the list
    [x, _, _, y] => x + y # first + fourth item in the list
    [] => 0 # match an empty list
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare Underscore CloseSquare OpFatArrow Int OpenSquare DoubleDot Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot CloseSquare OpFatArrow LowerIdent OpenSquare Underscore Comma Underscore Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma Underscore Comma Underscore Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "items")
))
~~~
# FORMATTED
~~~roc
match items
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:9 to 2:9

**Parse Error**
at 3:8 to 3:8

**Parse Error**
at 3:5 to 3:10

**Parse Error**
at 3:14 to 3:14

**Parse Error**
at 3:16 to 3:16

**Parse Error**
at 4:15 to 4:15

**Parse Error**
at 4:5 to 4:17

**Parse Error**
at 4:17 to 4:17

**Parse Error**
at 5:19 to 5:19

**Parse Error**
at 6:18 to 6:18

**Parse Error**
at 7:8 to 7:8

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
