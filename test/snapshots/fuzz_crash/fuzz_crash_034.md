# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]0 f
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare Int LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []0
f
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_034.md:1:9:1:10:**
```roc
module[]0 f
```
        ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_034.md:1:11:1:12:**
```roc
module[]0 f
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
