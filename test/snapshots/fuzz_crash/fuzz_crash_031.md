# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# TOKENS
~~~text
LowerIdent OpenSquare CloseSquare BlankLine LineComment LowerIdent LowerIdent OpAssign MalformedSingleQuoteUnclosed ~~~
# PARSE
~~~clojure
(block
  (lc "mule")
  (list_literal)
  (lc "vavar")
  (binop_equals
    (lc "t")
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
mule
[]
#el
vavar
t = '
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_031.md:1:1:1:5
PARSE ERROR - fuzz_crash_031.md:1:6:1:7
PARSE ERROR - fuzz_crash_031.md:1:7:1:8
PARSE ERROR - fuzz_crash_031.md:4:1:4:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_031.md:4:10:4:11
UNRECOGNIZED SYNTAX - fuzz_crash_031.md:4:10:4:11
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_031.md:4:10:4:11:**
```roc
vavar t= '
```
         ^


**UNDEFINED VARIABLE**
Nothing is named **mule** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_031.md:1:1:1:5:**
```roc
mule []
```
^^^^


**UNDEFINED VARIABLE**
Nothing is named **vavar** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_031.md:4:1:4:6:**
```roc
vavar t= '
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_031.md:4:7:4:8:**
```roc
vavar t= '
```
      ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "mule")
  (Expr.list_literal)
  (Expr.lookup "vavar")
  (Stmt.assign
    (pattern (Patt.ident "t"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 -> #9)
(var #3 _)
(var #4 -> #10)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 List #8)
(var #10 _)
~~~
# TYPES
~~~roc
t : _a
~~~
