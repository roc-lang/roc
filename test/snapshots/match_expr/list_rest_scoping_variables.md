# META
~~~ini
description=List rest patterns with proper variable scoping across branches
type=expr
~~~
# SOURCE
~~~roc
match data {
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (double_dot_lc "items")
      )
      (block
        (num_literal_i32 1)
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (unary_double_dot <unary>)
          )
          (lc "first")
        )
        (binop_thick_arrow
          (list_literal
            (unary_double_dot <unary>)
            (lc "last")
          )
          (lc "last")
        )
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (unary_double_dot <unary>)
            (lc "last")
          )
          (binop_plus
            (lc "first")
            (lc "last")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match data
	[..items] => 
		1
		[first, ..items] => first
		[..items, last] => last
		[first, ..items, last] => first + last
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_rest_scoping_variables.md:1:7:1:11:**
```roc
match data {
```
      ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping_variables.md:2:5:5:43:**
```roc
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
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
