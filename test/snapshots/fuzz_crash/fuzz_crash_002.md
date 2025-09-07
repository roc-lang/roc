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
                  (malformed)
                  (malformed)
                )
                (malformed)
              )
              (malformed)
            )
            (malformed)
          )
          (malformed)
        )
        (malformed)
      )
      (malformed)
    )
  )
  (lc "le")
  (list_literal
    (malformed)
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
MISSING HEADER - fuzz_crash_002.md:1:1:1:5
PARSE ERROR - fuzz_crash_002.md:1:5:1:6
PARSE ERROR - fuzz_crash_002.md:1:6:1:7
PARSE ERROR - fuzz_crash_002.md:1:7:1:8
PARSE ERROR - fuzz_crash_002.md:1:8:1:9
PARSE ERROR - fuzz_crash_002.md:1:9:1:10
PARSE ERROR - fuzz_crash_002.md:1:10:1:11
PARSE ERROR - fuzz_crash_002.md:1:11:1:12
PARSE ERROR - fuzz_crash_002.md:1:12:1:13
PARSE ERROR - fuzz_crash_002.md:1:13:1:14
PARSE ERROR - fuzz_crash_002.md:1:14:1:15
PARSE ERROR - fuzz_crash_002.md:1:15:1:16
PARSE ERROR - fuzz_crash_002.md:1:16:1:17
PARSE ERROR - fuzz_crash_002.md:1:17:1:18
PARSE ERROR - fuzz_crash_002.md:1:18:1:19
PARSE ERROR - fuzz_crash_002.md:1:19:1:20
PARSE ERROR - fuzz_crash_002.md:1:20:1:21
PARSE ERROR - fuzz_crash_002.md:1:21:1:23
PARSE ERROR - fuzz_crash_002.md:1:23:1:24
PARSE ERROR - fuzz_crash_002.md:1:24:1:25
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


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_002.md:1:6:1:21:**
```roc
modu:;::::::::::::::le[%
```
     ^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_002.md:1:1:1:5:**
```roc
modu:;::::::::::::::le[%
```
^^^^


**UNDEFINED VARIABLE**
Nothing is named **le** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_002.md:1:21:1:23:**
```roc
modu:;::::::::::::::le[%
```
                    ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "modu"))
    (type type_16)
  )
  (Expr.lookup "le")
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 22
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
~~~
# TYPES
~~~roc
modu : _a
~~~
