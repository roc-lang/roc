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
(match
  (scrutinee     (lc "items")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (lc "first")
        (unary_double_dot <unary>)
        (lc "last")
      )
      (block
        (binop_plus
          (lc "first")
          (lc "last")
        )
        (list_literal
          (lc "a")
          (lc "b")
          (unary_double_dot <unary>)
        )
        (tuple_literal
          (lc "middle")
          (lc "x")
          (lc "y")
        )
        (binop_thick_arrow
          (malformed malformed:expr_unexpected_token)
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
        (binop_thick_arrow
          (list_literal
            (lc "single")
          )
          (lc "single")
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
match items
	[first, .., last] => 
		first + last
		[a, b, ..as ]
		(middle, x, y)
		] => ((a + b) + x) + y
		[single] => single
		[] => 0
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**middle_rest.md:3:15:3:18:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
              ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**middle_rest.md:3:5:3:18:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
    ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**middle_rest.md:3:30:3:32:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                             ^^


**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**middle_rest.md:2:5:5:12:**
```roc
    [first, .., last] => first + last
    [a, b, .. as middle, x, y] => a + b + x + y  
    [single] => single
    [] => 0
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_c")
~~~
# TYPES
~~~roc
_c
~~~
