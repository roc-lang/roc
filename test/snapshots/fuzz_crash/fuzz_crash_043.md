# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0}0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseCurly Int ~~~
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
app { f: "" platform [] }{
	o : 0
}
0
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_043.md:1:20:2:5:**
```roc
app[]{f:platform""}{
o:0}0
```


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_043.md:2:5:2:6:**
```roc
o:0}0
```
    ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
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
