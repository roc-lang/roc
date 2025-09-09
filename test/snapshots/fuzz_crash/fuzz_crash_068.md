# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({0})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound OpenCurly Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (block
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
module []

{
	0
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_068.md:1:9:1:10
PARSE ERROR - fuzz_crash_068.md:1:10:1:11
PARSE ERROR - fuzz_crash_068.md:1:11:1:12
PARSE ERROR - fuzz_crash_068.md:1:12:1:13
PARSE ERROR - fuzz_crash_068.md:1:13:1:14
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.num_literal_i32 0)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 Num *)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
