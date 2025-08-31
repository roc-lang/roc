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
NIL
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
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
