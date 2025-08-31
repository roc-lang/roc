# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwAs UpperIdent KwIf Int OpenCurly CloseCurly KwElse OpOr Int ~~~
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
app { f: "" platform [] }import B as G
if 0 {} else ||
0
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **||** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_059.md:2:13:2:15:**
```roc
G	if 0{}else||0
```
 	          ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_059.md:2:3:2:15:**
```roc
G	if 0{}else||0
```
 	^^^^^^^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_059.md:2:15:2:16:**
```roc
G	if 0{}else||0
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
