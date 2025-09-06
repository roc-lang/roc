# META
~~~ini
description=if_then_else (7)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else {
	2
}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse OpenCurly Int CloseCurly ~~~
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
if bool
	{
		1
	}
else {
	2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_7.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_7.md:1:4:1:8:**
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
