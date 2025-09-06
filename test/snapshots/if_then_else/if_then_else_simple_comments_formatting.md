# META
~~~ini
description=if_then_else (5)
type=expr
~~~
# SOURCE
~~~roc
if bool { # Comment after then open
	A # Comment after expr
} else B
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly LineComment UpperIdent LineComment CloseCurly KwElse UpperIdent ~~~
# PARSE
~~~clojure
(if_else
  (condition     (lc "bool")
)
  (then     (block
      (uc "A")
    )
)
  (else     (uc "B")
))
~~~
# FORMATTED
~~~roc
if bool
	{
		# Comment after then open
		A
	}
else # Comment after expr
B
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_simple_comments_formatting.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_simple_comments_formatting.md:1:4:1:8:**
```roc
if bool { # Comment after then open
```
   ^^^^


# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
