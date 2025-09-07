# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
 f{o,
     ]

foo =

    "onmo %
~~~
# TOKENS
~~~text
LowerIdent OpenCurly LowerIdent Comma CloseSquare BlankLine LowerIdent OpAssign BlankLine MalformedString ~~~
# PARSE
~~~clojure
(block
  (lc "f")
  (record_literal
    (lc "o")
    (malformed)
  )
  (binop_equals
    (lc "foo")
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
f
{
	o,
}
foo = "onmo %
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_009.md:1:2:1:3
PARSE ERROR - fuzz_crash_009.md:1:3:1:4
PARSE ERROR - fuzz_crash_009.md:1:4:1:5
PARSE ERROR - fuzz_crash_009.md:1:5:1:6
PARSE ERROR - fuzz_crash_009.md:2:6:2:7
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_009.md:2:6:4:1:**
```roc
     ]

foo =
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_009.md:1:3:4:1:**
```roc
 f{o,
     ]

foo =
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"onmo %** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_009.md:6:5:6:12:**
```roc
    "onmo %
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **f** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_009.md:1:2:1:3:**
```roc
 f{o,
```
 ^


**UNDEFINED VARIABLE**
Nothing is named **o** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_009.md:1:4:1:5:**
```roc
 f{o,
```
   ^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_009.md:4:1:4:4:**
```roc
foo =
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "f")
  (Expr.record_literal
    (Expr.lookup "o")
    (Expr.malformed)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #10)
(var #5 -> #11)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 {})
(var #10 record)
(var #11 _)
~~~
# TYPES
~~~roc
foo : _a
~~~
