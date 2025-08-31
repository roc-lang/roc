# META
~~~ini
description=Match expression with underscore patterns in list matching
type=expr
~~~
# SOURCE
~~~roc
match items {
    [_] => 1 # pattern match on a list with a single (ignored) element
    [.., last] => last # pattern match on the last item in the list
    [first, ..] => first # pattern match on the first item in the list
    [_, _, third] => third # pattern match on the third item in the list
    [x, _, _, y] => x + y # first + fourth item in the list
    [] => 0 # match an empty list
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare Underscore CloseSquare OpFatArrow Int LineComment OpenSquare DoubleDot Comma LowerIdent CloseSquare OpFatArrow LowerIdent LineComment OpenSquare LowerIdent Comma DoubleDot CloseSquare OpFatArrow LowerIdent LineComment OpenSquare Underscore Comma Underscore Comma LowerIdent CloseSquare OpFatArrow LowerIdent LineComment OpenSquare LowerIdent Comma Underscore Comma Underscore Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent LineComment OpenSquare CloseSquare OpFatArrow Int LineComment CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "items")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (underscore)
      )
      (block
        (num_literal_i32 1)
        (list_literal
          (unary_double_dot <unary>)
        )
        (lc "last")
        (binop_thick_arrow
          (malformed malformed:expr_unexpected_token)
          (lc "last")
        )
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (unary_double_dot <unary>)
          )
          (lc "first")
        )
        (binop_thick_arrow
          (list_literal
            (underscore)
            (underscore)
            (lc "third")
          )
          (lc "third")
        )
        (binop_thick_arrow
          (list_literal
            (lc "x")
            (underscore)
            (underscore)
            (lc "y")
          )
          (binop_plus
            (lc "x")
            (lc "y")
          )
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
	[_] => 
		1
		[.., ]
		last
		] => last
		[first, ..] ] => first
		[_, _, third] => third
		[x, _, _, y] => x + y
		[] => 0
# pattern match on the first item in the list
# pattern match on the third item in the list
# first + fourth item in the list
# match an empty list
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_underscore_patterns.md:3:8:3:10:**
```roc
    [.., last] => last # pattern match on the last item in the list
```
       ^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**list_underscore_patterns.md:3:5:3:10:**
```roc
    [.., last] => last # pattern match on the last item in the list
```
    ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_underscore_patterns.md:3:14:3:16:**
```roc
    [.., last] => last # pattern match on the last item in the list
```
             ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_underscore_patterns.md:4:15:4:17:**
```roc
    [first, ..] => first # pattern match on the first item in the list
```
              ^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**list_underscore_patterns.md:4:5:4:17:**
```roc
    [first, ..] => first # pattern match on the first item in the list
```
    ^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_underscore_patterns.md:2:5:7:12:**
```roc
    [_] => 1 # pattern match on a list with a single (ignored) element
    [.., last] => last # pattern match on the last item in the list
    [first, ..] => first # pattern match on the first item in the list
    [_, _, third] => third # pattern match on the third item in the list
    [x, _, _, y] => x + y # first + fourth item in the list
    [] => 0 # match an empty list
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
