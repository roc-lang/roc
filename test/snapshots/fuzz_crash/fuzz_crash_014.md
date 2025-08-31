# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# TOKENS
~~~text
MalformedNumberNoDigits Dot Int MalformedNumberNoDigits LowerIdent Int LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (malformed malformed:expr_unexpected_token)
    (num_literal_i32 0)
  )
  (malformed malformed:expr_unexpected_token)
  (lc "u22")
  (num_literal_i32 0)
  (lc "u22")
)
~~~
# FORMATTED
~~~roc
 | 0
0b
u22
0
u22
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **0b** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_014.md:2:1:2:3:**
```roc
0bu22
```
^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_014.md:1:3:1:5:**
```roc
0b.0
```
  ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_014.md:2:1:2:3:**
```roc
0bu22
```
^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_014.md:2:3:2:6:**
```roc
0bu22
```
  ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_014.md:3:1:3:2:**
```roc
0u22
```
^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_014.md:3:2:3:5:**
```roc
0u22
```
 ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
