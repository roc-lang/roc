# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	A
} else 2
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly UpperIdent CloseCurly KwElse Int ~~~
# PARSE
~~~clojure
(if_else
  (condition     (lc "bool")
)
  (then     (block
      (uc "A")
    )
)
  (else     (num_literal_i32 2)
))
~~~
# FORMATTED
~~~roc
if bool
	{
		A
	}
else 2
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_simple_block_formatting.md:1:4:1:8:**
```roc
if bool {
```
   ^^^^


# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
