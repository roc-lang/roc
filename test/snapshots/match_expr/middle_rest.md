# META
~~~ini
description=Match expression with rest patterns in middle position
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, .., last] => first + last
    [a, b, .. as middle, x, y] => a + b + x + y  
    [single] => single
    [] => 0
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare LowerIdent Comma LowerIdent Comma DoubleDot KwAs LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(binop_thick_arrow
  (match
    (scrutinee       (lc "items")
))
  (binop_plus
    (binop_plus
      (binop_plus
        (lc "a")
        (lc "b")
      )
      (lc "x")
    )
    (lc "y")
  )
)
~~~
# FORMATTED
~~~roc
match items
 => ((a + b) + x) + y
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:15 to 2:17

**Parse Error**
at 2:5 to 2:17

**Parse Error**
at 2:21 to 2:23

**Parse Error**
at 2:23 to 2:26

**Parse Error**
at 3:15 to 3:18

**Parse Error**
at 3:5 to 3:18

**Parse Error**
at 1:13 to 3:30

**Parse Error**
at 1:13 to 3:32

**Unsupported Node**
at 3:32 to 3:34

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
