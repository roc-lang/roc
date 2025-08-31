# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# TOKENS
~~~text
LowerIdent OpColon MalformedUnknownToken OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon LowerIdent OpenSquare MalformedUnknownToken ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "modu")
    (binop_colon
      (binop_colon
        (binop_colon
          (binop_colon
            (binop_colon
              (binop_colon
                (binop_colon
                  (malformed malformed:expr_unexpected_token)
                  (malformed malformed:expr_unexpected_token)
                )
                (malformed malformed:expr_unexpected_token)
              )
              (malformed malformed:expr_unexpected_token)
            )
            (malformed malformed:expr_unexpected_token)
          )
          (malformed malformed:expr_unexpected_token)
        )
        (malformed malformed:expr_unexpected_token)
      )
      (malformed malformed:expr_unexpected_token)
    )
  )
  (lc "le")
  (list_literal
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
modu : ; : : : : : : : : : : : : : :
le
[%]
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **;** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:6:1:7:**
```roc
modu:;::::::::::::::le[%
```
     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:8:1:9:**
```roc
modu:;::::::::::::::le[%
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:10:1:11:**
```roc
modu:;::::::::::::::le[%
```
         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:12:1:13:**
```roc
modu:;::::::::::::::le[%
```
           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:14:1:15:**
```roc
modu:;::::::::::::::le[%
```
             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:16:1:17:**
```roc
modu:;::::::::::::::le[%
```
               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:18:1:19:**
```roc
modu:;::::::::::::::le[%
```
                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:20:1:21:**
```roc
modu:;::::::::::::::le[%
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **%** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_002.md:1:24:1:25:**
```roc
modu:;::::::::::::::le[%
```
                       ^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_002.md:1:23:1:25:**
```roc
modu:;::::::::::::::le[%
```
                      ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_002.md:1:21:1:23:**
```roc
modu:;::::::::::::::le[%
```
                    ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_002.md:1:23:1:25:**
```roc
modu:;::::::::::::::le[%
```
                      ^^


# CANONICALIZE
~~~clojure
(Expr.block
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
