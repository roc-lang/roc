# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow Int LineComment OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow Int LineComment OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow Int LineComment CloseCurly ~~~
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
        (num_literal_i32 0)
        (binop_thick_arrow
          (list_literal
            (unary_double_dot <unary_op>)
            (lc "last")
          )
          (num_literal_i32 1)
        )
        (binop_thick_arrow
          (list_literal
            (lc "x")
            (unary_double_dot <unary_op>)
            (lc "y")
          )
          (num_literal_i32 2)
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match items
	[first, ..rest] => 
		0
		 # invalid rest pattern should error
[..rest, last] => 1
		 # invalid rest pattern should error
[x, ..rest, y] => 2
# invalid rest pattern should error
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_rest_invalid.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_invalid.md:2:5:4:24:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
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
