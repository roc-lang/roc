# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{
 
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []{}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_035.md:1:9:2:2:**
```roc
module[]{
 
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_035.md:1:9:2:2:**
```roc
module[]{
 
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
