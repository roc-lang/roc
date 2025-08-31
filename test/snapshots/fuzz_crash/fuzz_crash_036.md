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
~~~
# FORMATTED
~~~roc
module []

{
	B
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_036.md:1:9:1:11:**
```roc
module[]{B
```
        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_036.md:1:9:1:11:**
```roc
module[]{B
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
