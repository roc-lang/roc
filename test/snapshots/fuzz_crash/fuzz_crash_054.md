# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import S exposing[c as
f]
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent CloseSquare ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (uc "S")
      (list_literal
        (lc "c")
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (lc "f")
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

import S exposing [c]
f
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:40 to 1:40

**Parse Error**
at 2:2 to 2:2

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
