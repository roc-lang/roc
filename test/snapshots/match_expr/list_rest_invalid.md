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
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:4:9:4:15
UNDEFINED VARIABLE - list_rest_invalid.md:1:7:1:12
UNUSED VARIABLE - list_rest_invalid.md:2:6:2:11
UNUSED VARIABLE - list_rest_invalid.md:2:15:2:15
UNUSED VARIABLE - list_rest_invalid.md:3:8:3:8
UNUSED VARIABLE - list_rest_invalid.md:3:14:3:18
UNUSED VARIABLE - list_rest_invalid.md:4:6:4:7
UNUSED VARIABLE - list_rest_invalid.md:4:11:4:11
UNUSED VARIABLE - list_rest_invalid.md:4:17:4:18
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
; Total type variables: 22
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
~~~
# TYPES
~~~roc
~~~
