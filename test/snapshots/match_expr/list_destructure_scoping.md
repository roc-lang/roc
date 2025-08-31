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
NIL
# PROBLEMS
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
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
