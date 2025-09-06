# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare OpenSquare LowerIdent CloseSquare Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpBinaryMinus LowerIdent OpenSquare LowerIdent Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpStar LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "nestedList")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (list_literal
          (lc "x")
        )
        (list_literal
          (lc "y")
        )
      )
      (block
        (binop_plus
          (lc "x")
          (lc "y")
        )
        (binop_thick_arrow
          (list_literal
            (list_literal
              (lc "x")
              (lc "y")
            )
          )
          (binop_minus
            (lc "x")
            (lc "y")
          )
        )
        (binop_thick_arrow
          (list_literal
            (lc "x")
            (list_literal
              (lc "y")
            )
          )
          (binop_star
            (lc "x")
            (lc "y")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match nestedList
	[[x], [y]] => 
		x + y
		[[x, y]] => x - y
		[x, [y]] => x * y
~~~
# EXPECTED
UNDEFINED VARIABLE - nested_list_scoping.md:1:7:1:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **nestedList** in this scope.
Is there an **import** or **exposing** missing up-top?

**nested_list_scoping.md:1:7:1:17:**
```roc
match nestedList {
```
      ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nested_list_scoping.md:2:5:4:22:**
```roc
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 29
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
(var #28 _)
~~~
# TYPES
~~~roc
~~~
