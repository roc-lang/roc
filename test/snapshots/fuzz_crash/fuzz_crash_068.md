# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({0})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound OpenCurly Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []{
	0
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_068.md:1:10:1:13:**
```roc
module[]({0})
```
         ^^^


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
