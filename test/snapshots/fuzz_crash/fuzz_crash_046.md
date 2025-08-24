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
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent UpperIdent ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "f")
  )
  (uc "S")
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
# PROBLEMS
**Unsupported Node**
at 1:20 to 1:28

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
