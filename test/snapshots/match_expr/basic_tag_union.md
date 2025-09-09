# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpThinArrow Int UpperIdent OpThinArrow Int UpperIdent OpThinArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "color")
)
  (branch1     (binop_thick_arrow
      (uc "Red")
      (num_literal_i32 1)
    )
)
  (branch2     (binop_thick_arrow
      (uc "Blue")
      (num_literal_i32 2)
    )
)
  (branch3     (binop_thick_arrow
      (uc "Green")
      (str_literal_small "3")
    )
))
~~~
# FORMATTED
~~~roc
match color
	Red => 1
	Blue => 2
	Green => "3"
~~~
# EXPECTED
UNDEFINED VARIABLE - basic_tag_union.md:1:7:1:12
INCOMPATIBLE MATCH BRANCHES - basic_tag_union.md:1:1:1:1
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **color** in this scope.
Is there an **import** or **exposing** missing up-top?

**basic_tag_union.md:1:7:1:12:**
```roc
match color {
```
      ^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Num *)
(var #4 _)
(var #5 _)
(var #6 Num *)
(var #7 _)
(var #8 _)
(var #9 Str)
(var #10 _)
(var #11 _)
~~~
# TYPES
~~~roc
~~~
