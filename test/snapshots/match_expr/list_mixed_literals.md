# META
~~~ini
description=Match expression with mixed literal and variable patterns in lists
type=expr
~~~
# SOURCE
~~~roc
match sequence {
    [0, count] => count
    [1, x, 3] => x
    [42, value] => value
    [first, 99] => first
    [] => 0
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare Int Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma LowerIdent Comma Int CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma Int CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "sequence")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (num_literal_i32 0)
        (lc "count")
      )
      (block
        (lc "count")
        (binop_thick_arrow
          (list_literal
            (num_literal_i32 1)
            (lc "x")
            (num_literal_i32 3)
          )
          (lc "x")
        )
        (binop_thick_arrow
          (list_literal
            (num_literal_i32 42)
            (lc "value")
          )
          (lc "value")
        )
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (num_literal_i32 99)
          )
          (lc "first")
        )
        (binop_thick_arrow
          (list_literal)
          (num_literal_i32 0)
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match sequence
	[0, count] => 
		count
		[1, x, 3] => x
		[42, value] => value
		[first, 99] => first
		[] => 0
~~~
# EXPECTED
UNDEFINED VARIABLE - list_mixed_literals.md:1:7:1:15
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **sequence** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_mixed_literals.md:1:7:1:15:**
```roc
match sequence {
```
      ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_mixed_literals.md:2:5:6:12:**
```roc
    [0, count] => count
    [1, x, 3] => x
    [42, value] => value
    [first, 99] => first
    [] => 0
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 28
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
~~~
# TYPES
~~~roc
~~~
