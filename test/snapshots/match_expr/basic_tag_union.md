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
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow String CloseCurly ~~~
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
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**basic_tag_union.md:2:2:2:10:**
```roc
	Red => 1
```
	^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**basic_tag_union.md:3:2:3:6:**
```roc
	Blue => 2
```
	^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**basic_tag_union.md:4:2:4:14:**
```roc
	Green => "3"
```
	^^^^^^^^^^^^


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
