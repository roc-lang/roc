# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{B
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly UpperIdent ~~~
# PARSE
~~~clojure
(block
  (block
    (uc "B")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_036.md:1:9:1:10
PARSE ERROR - fuzz_crash_036.md:1:11:1:11
# PROBLEMS
**Parse Error**
at 1:9 to 1:11

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
