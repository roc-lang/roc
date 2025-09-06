# META
~~~ini
description=if_then_else (15)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool # Comment after cond
		{ # Comment after then open
			1
		} # Comment after then close
			else # Comment after else
				{ # Comment else open
					2
				}
~~~
# TOKENS
~~~text
KwIf LineComment LowerIdent LineComment OpenCurly LineComment Int CloseCurly LineComment KwElse LineComment OpenCurly LineComment Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else
  (condition     (lc "bool")
)
  (then     (block
      (num_literal_i32 1)
    )
)
  (else     (block
      (num_literal_i32 2)
    )
))
~~~
# FORMATTED
~~~roc
if # Comment after if
bool
	 # Comment after cond
{
		# Comment after then open
		1
	}
else # Comment after then close
# Comment after else
{
	# Comment else open
	2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_comments_complex.md:2:2:2:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_comments_complex.md:2:2:2:6:**
```roc
	bool # Comment after cond
```
	^^^^


# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
~~~
