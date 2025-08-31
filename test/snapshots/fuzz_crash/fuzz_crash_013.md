# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0{
~~~
# TOKENS
~~~text
Int OpenCurly ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (block)
)
~~~
# FORMATTED
~~~roc
0
{}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_013.md:1:2:1:3:**
```roc
0{
```
 ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_013.md:1:1:1:2:**
```roc
0{
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_013.md:1:2:1:3:**
```roc
0{
```
 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
