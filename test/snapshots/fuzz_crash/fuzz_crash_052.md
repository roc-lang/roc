# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import
S
0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []import S
0
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_052.md:3:1:3:2:**
```roc
0
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
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
