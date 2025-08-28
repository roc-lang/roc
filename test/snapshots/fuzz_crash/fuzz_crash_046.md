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
app
{
	f: "" platform [],
}

import f
S
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
