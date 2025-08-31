# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
# TOKENS
~~~text
Int Dot Int Int Underscore Int Int LowerIdent Dot Int Int Underscore ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (num_literal_big big:<idx:0>)
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (underscore)
  (num_literal_i32 0)
  (num_literal_i32 0)
  (binop_pipe
    (lc "u8")
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (underscore)
)
~~~
# FORMATTED
~~~roc
0o0.0 | 0
0_0
_
0
0
u8 | 0
0_
_
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_015.md:1:1:1:6:**
```roc
0o0.0
```
^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_015.md:2:1:2:2:**
```roc
0_0
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_015.md:2:2:2:3:**
```roc
0_0
```
 ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_015.md:2:3:2:4:**
```roc
0_0
```
  ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_015.md:3:1:3:2:**
```roc
0u8.0
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_015.md:3:2:3:6:**
```roc
0u8.0
```
 ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_015.md:4:1:4:2:**
```roc
0_
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_015.md:4:2:4:3:**
```roc
0_
```
 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
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
