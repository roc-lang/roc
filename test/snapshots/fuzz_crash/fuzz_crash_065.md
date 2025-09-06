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
(module-header)
(block
  (block
    (uc "R")
  )
)
~~~
# FORMATTED
~~~roc
module []

{
	R
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_065.md:1:9:1:10
PARSE ERROR - fuzz_crash_065.md:1:11:1:12
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.tag_no_args)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
