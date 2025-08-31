# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "items")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (lc "first")
        (double_dot_lc "rest")
      )
      (block
        (binop_plus
          (lc "first")
          (num_literal_i32 1)
        )
        (binop_thick_arrow
          (list_literal
            (unary_double_dot <unary>)
            (lc "last")
          )
          (binop_plus
            (lc "last")
            (num_literal_i32 2)
          )
        )
        (binop_thick_arrow
          (list_literal
            (lc "x")
            (unary_double_dot <unary>)
            (lc "y")
          )
          (binop_plus
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
match items
	[first, ..rest] => 
		first + 1
		[..rest, last] => last + 2
		[x, ..rest, y] => x + y
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping.md:2:5:4:28:**
```roc
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
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
