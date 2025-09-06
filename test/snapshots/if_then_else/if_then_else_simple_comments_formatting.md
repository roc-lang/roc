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
~~~
# TYPES
~~~roc
# No header found
~~~
