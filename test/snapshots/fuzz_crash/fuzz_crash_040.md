# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

{
	o : 0
	)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_040.md:2:4:2:5:**
```roc
o:0)
```
   ^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_040.md:1:20:2:5:**
```roc
app[]{f:platform""}{
o:0)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_040.md:1:20:2:5:**
```roc
app[]{f:platform""}{
o:0)
```


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
