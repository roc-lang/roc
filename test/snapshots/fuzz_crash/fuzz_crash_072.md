# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({})(!{0})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound OpenCurly CloseCurly CloseRound OpenRound OpBang OpenCurly Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (record_literal)
    (unary_not <unary>)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_072.md:1:9:1:10
PARSE ERROR - fuzz_crash_072.md:1:10:1:11
PARSE ERROR - fuzz_crash_072.md:1:11:1:12
PARSE ERROR - fuzz_crash_072.md:1:12:1:13
PARSE ERROR - fuzz_crash_072.md:1:13:1:14
PARSE ERROR - fuzz_crash_072.md:1:14:1:15
PARSE ERROR - fuzz_crash_072.md:1:15:1:16
PARSE ERROR - fuzz_crash_072.md:1:16:1:17
PARSE ERROR - fuzz_crash_072.md:1:17:1:18
PARSE ERROR - fuzz_crash_072.md:1:18:1:19
# PROBLEMS
**Unsupported Node**
at 1:10 to 1:19

# CANONICALIZE
~~~clojure
(Expr.block
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
