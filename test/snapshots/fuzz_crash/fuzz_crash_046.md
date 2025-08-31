# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import fS
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent MalformedUnknownToken UpperIdent ~~~
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
app { f: "" platform [] }import f

S
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_046.md:1:28:1:29:**
```roc
app[]{f:platform""}import fS
```
                           ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_046.md:1:28:1:29:**
```roc
app[]{f:platform""}import fS
```
                           ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_046.md:1:29:1:30:**
```roc
app[]{f:platform""}import fS
```
                            ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
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
