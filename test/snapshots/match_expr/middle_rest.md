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
        (unary_double_dot <unary_op>)
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
          (unary_double_dot <unary_op>)
        )
        (tuple_literal
          (lc "middle")
          (lc "x")
          (lc "y")
        )
        (binop_thick_arrow
          (malformed)
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
UNDEFINED VARIABLE - middle_rest.md:1:7:1:12
UNUSED VARIABLE - middle_rest.md:1:1:1:1
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

**middle_rest.md:2:13:2:15:**
```roc
    [first, .., last] => first + last
```
            ^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:6:3:7:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
     ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:9:3:10:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
        ^


**UNDEFINED VARIABLE**
Nothing is named **middle** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:18:3:24:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                 ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:26:3:27:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                         ^


**UNDEFINED VARIABLE**
Nothing is named **y** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:29:3:30:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                            ^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:35:3:36:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                                  ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:39:3:40:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                                      ^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:43:3:44:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                                          ^


**UNDEFINED VARIABLE**
Nothing is named **y** in this scope.
Is there an **import** or **exposing** missing up-top?

**middle_rest.md:3:47:3:48:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                                              ^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 37
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
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 Num *)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
~~~
# TYPES
~~~roc
last : _c
single : _c
first : _c
~~~
