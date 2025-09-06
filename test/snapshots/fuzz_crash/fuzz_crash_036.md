# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{B
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly UpperIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (block
    (uc "B")
  )
)
~~~
# FORMATTED
~~~roc
module []

{
	B
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_036.md:1:9:1:10
PARSE ERROR - fuzz_crash_036.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_036.md:1:9:1:11:**
```roc
module[]{B
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.tag_no_args)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
