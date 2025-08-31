# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma Underscore CloseRound LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon OpenRound MalformedUnknownToken UpperIdent Comma UpperIdent OpArrow UpperIdent CloseRound LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : 
U8
, 
U16
-> 
U32
)
main! : List String -> Result({}, _)
tag_tuple : Value(a, b, c)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:6:12:7:1:**
```roc
add_one : (
U8, U16 -> U32)
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:7:3:7:5:**
```roc
U8, U16 -> U32)
```
  ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **-> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:7:9:7:12:**
```roc
U8, U16 -> U32)
```
        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_048.md:7:15:8:1:**
```roc
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_048.md:7:1:7:3:**
```roc
U8, U16 -> U32)
```
^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_048.md:7:3:7:5:**
```roc
U8, U16 -> U32)
```
  ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_048.md:7:5:7:8:**
```roc
U8, U16 -> U32)
```
    ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_048.md:7:9:7:12:**
```roc
U8, U16 -> U32)
```
        ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_048.md:7:12:7:15:**
```roc
U8, U16 -> U32)
```
           ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_048.md:7:15:8:1:**
```roc
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "foo")
    (type uc)
  )
  (Stmt.type_anno
    (name "bar")
    (type apply_uc)
  )
  (Stmt.type_anno
    (name "biz")
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name "add_one")
    (type malformed)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "main")
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name "tag_tuple")
    (type apply_uc)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
