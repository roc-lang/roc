# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import fS
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent MalformedUnknownToken UpperIdent ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
(block
  (import
    (lc "f")
  )
  (malformed)
  (uc "S")
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

import f

S
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_046.md:1:20:1:26
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_046.md:1:28:1:29:**
```roc
app[]{f:platform""}import fS
```
                           ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.malformed)
  (Expr.tag_no_args)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
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
~~~
# TYPES
~~~roc
~~~
