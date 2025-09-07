# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import u.R}g:r->R.a.E
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent CloseCurly LowerIdent OpColon LowerIdent OpArrow UpperIdent Dot LowerIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_dot
      (lc "u")
      (uc "R")
    )
  )
  (malformed)
  (binop_colon
    (lc "g")
    (binop_arrow_call
      (lc "r")
      (binop_dot
        (binop_dot
          (uc "R")
          (dot_lc "a")
        )
        (uc "E")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import u.R
}
g : r -> R..a.E
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_042.md:1:19:1:20
MODULE NOT FOUND - fuzz_crash_042.md:1:9:1:19
MODULE NOT IMPORTED - fuzz_crash_042.md:1:25:1:30
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_042.md:1:19:1:20:**
```roc
module[]import u.R}g:r->R.a.E
```
                  ^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_042.md:1:26:1:28:**
```roc
module[]import u.R}g:r->R.a.E
```
                         ^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_042.md:1:20:1:21:**
```roc
module[]import u.R}g:r->R.a.E
```
                   ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "g"))
    (type type_13)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 17
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
~~~
# TYPES
~~~roc
g : _b
~~~
