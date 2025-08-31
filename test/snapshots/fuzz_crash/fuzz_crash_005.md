# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu
~~~
# TOKENS
~~~text
LowerIdent ~~~
# PARSE
~~~clojure
(block
  (lc "modu")
)
~~~
# FORMATTED
~~~roc
modu
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_005.md:1:1:1:5:**
```roc
modu
```
^^^^


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
