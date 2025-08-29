# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwAs UpperIdent KwIf Int OpenCurly CloseCurly KwElse OpOr Int ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_as
      (uc "B")
      (uc "G")
    )
  )
  (if_else <6 branches>)
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

import B as G
if 0 {  } else 
0
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:3 to 2:7

**Parse Error**
at 2:13 to 2:13

**Unsupported Node**
at 1:20 to 2:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.if_else)
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
