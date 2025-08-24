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
    (uc "S")
    (lc "c")
  )
  (malformed malformed:expr_unexpected_token)
  (lc "f")
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - fuzz_crash_054.md:1:20:2:3
# PROBLEMS
**Parse Error**
at 1:40 to 1:40

**Parse Error**
at 2:2 to 2:2

**Unsupported Node**
at 1:20 to 1:39

**Unsupported Node**
at 1:40 to 1:40

**Unsupported Node**
at 2:2 to 2:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "f")
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
