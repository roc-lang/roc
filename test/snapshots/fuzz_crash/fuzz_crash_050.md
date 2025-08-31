# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]_0={
)
 
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare Underscore Int OpAssign OpenCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []_
0 = {
	)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
 ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_050.md:2:1:3:2:**
```roc
)
 
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_050.md:1:12:3:2:**
```roc
module[]_0={
)
 
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_050.md:1:9:1:10:**
```roc
module[]_0={
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.num_literal_i32))
    (Expr.block
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
