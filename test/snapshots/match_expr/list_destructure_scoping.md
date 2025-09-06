# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "list")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (lc "first")
      )
      (block
        (lc "first")
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (lc "second")
          )
          (binop_plus
            (lc "first")
            (lc "second")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match list
	[first] => 
		first
		[first, second] => first + second
~~~
# EXPECTED
UNDEFINED VARIABLE - list_destructure_scoping.md:1:7:1:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_destructure_scoping.md:1:7:1:11:**
```roc
match list {
```
      ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_destructure_scoping.md:2:5:3:38:**
```roc
    [first] => first
    [first, second] => first + second
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 15
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
~~~
# TYPES
~~~roc
~~~
