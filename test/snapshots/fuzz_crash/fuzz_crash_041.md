# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpBar OpenRound Int Comma CloseRound OpOr OpBar Int ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_041.md:1:20:1:21
PARSE ERROR - fuzz_crash_041.md:1:21:1:22
PARSE ERROR - fuzz_crash_041.md:1:22:1:23
PARSE ERROR - fuzz_crash_041.md:1:23:1:24
PARSE ERROR - fuzz_crash_041.md:1:24:1:25
PARSE ERROR - fuzz_crash_041.md:1:25:1:26
PARSE ERROR - fuzz_crash_041.md:1:26:1:27
PARSE ERROR - fuzz_crash_041.md:1:27:1:28
PARSE ERROR - fuzz_crash_041.md:1:28:1:29
# PROBLEMS
**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:29 to 1:29

**Parse Error**
at 1:29 to 1:29

**Parse Error**
at 1:29 to 1:29

**Unsupported Node**
at 1:29 to 1:29

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
