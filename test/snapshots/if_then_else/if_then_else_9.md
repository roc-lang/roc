# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse KwIf Int OpenCurly LineComment UpperIdent CloseCurly KwElse OpenCurly LineComment Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else
  (condition     (lc "bool")
)
  (then     (block
      (num_literal_i32 1)
    )
)
  (else     (if_else
      (condition         (num_literal_i32 10)
)
      (then         (block
          (uc "A")
        )
)
      (else         (block
          (num_literal_i32 3)
        )
))
))
~~~
# FORMATTED
~~~roc
if bool
	{
		1
	}
else if 10
	{
		# Comment after else open
		A
	}
else {
	# Comment after else open
	3
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_9.md:1:4:1:8
INVALID IF CONDITION - if_then_else_9.md:3:11:3:11
INCOMPATIBLE IF BRANCHES - if_then_else_9.md:1:1:1:1
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_9.md:1:4:1:8:**
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
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 Num *)
(var #8 _)
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
~~~
