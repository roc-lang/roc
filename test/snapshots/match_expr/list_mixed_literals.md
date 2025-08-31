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
NIL
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
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
