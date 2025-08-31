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
foo =

"on        (string 'onmo %')))
~~~
# EXPECTED
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_010.md:1:1:1:2:**
```roc
H{o,
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_010.md:1:2:2:7:**
```roc
H{o,
    ]
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_010.md:2:6:3:1:**
```roc
    ]
foo =
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
