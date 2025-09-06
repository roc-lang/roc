# META
~~~ini
description=if_then_else (11)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool
		{
			1
		} else {
			2
		}
~~~
# TOKENS
~~~text
KwIf LineComment LowerIdent OpenCurly Int CloseCurly KwElse OpenCurly Int CloseCurly ~~~
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
	{
		1
	}
else {
	2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_comments_block.md:2:2:2:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_comments_block.md:2:2:2:6:**
```roc
	bool
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
