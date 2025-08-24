# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{R}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (block
    (uc "R")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_065.md:1:9:1:10
PARSE ERROR - fuzz_crash_065.md:1:11:1:12
# PROBLEMS
**Pattern in Expression Context**
at 1:10 to 1:11

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
