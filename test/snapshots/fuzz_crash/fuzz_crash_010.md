# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# TOKENS
~~~text
UpperIdent OpenCurly LowerIdent Comma MalformedUnknownToken CloseSquare LowerIdent OpAssign BlankLine MalformedString ~~~
# PARSE
~~~clojure
(block
  (uc "H")
  (record_literal
    (lc "o")
    (malformed)
  )
  (malformed)
  (binop_equals
    (lc "foo")
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
H
{ o }
]
foo = "on        (string 'onmo %')))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_010.md:1:1:1:2
PARSE ERROR - fuzz_crash_010.md:1:2:1:3
PARSE ERROR - fuzz_crash_010.md:1:3:1:4
PARSE ERROR - fuzz_crash_010.md:1:4:1:5
PARSE ERROR - fuzz_crash_010.md:2:6:2:7
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **  ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_010.md:2:3:2:6:**
```roc
    ]
```
  ^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_010.md:1:2:2:6:**
```roc
H{o,
    ]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_010.md:2:6:3:1:**
```roc
    ]
foo =
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"on        (string 'onmo %')))** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_010.md:5:5:5:35:**
```roc
    "on        (string 'onmo %')))
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **o** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_010.md:1:3:1:4:**
```roc
H{o,
```
  ^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_010.md:3:1:3:4:**
```roc
foo =
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.tag_no_args)
  (Expr.record_literal
    (Expr.lookup "o")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #11)
(var #5 _)
(var #6 -> #13)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 {})
(var #11 record)
(var #12 _)
(var #13 _)
~~~
# TYPES
~~~roc
foo : _a
~~~
