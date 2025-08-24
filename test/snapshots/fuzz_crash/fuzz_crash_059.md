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
    (uc "B")
    (uc "G")
  )
  (if_else <4 branches>)
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_059.md:2:3:2:5
PARSE ERROR - fuzz_crash_059.md:2:6:2:7
PARSE ERROR - fuzz_crash_059.md:2:7:2:8
PARSE ERROR - fuzz_crash_059.md:2:8:2:9
PARSE ERROR - fuzz_crash_059.md:2:9:2:13
PARSE ERROR - fuzz_crash_059.md:2:13:2:14
PARSE ERROR - fuzz_crash_059.md:2:14:2:15
PARSE ERROR - fuzz_crash_059.md:2:15:2:16
MODULE NOT FOUND - fuzz_crash_059.md:1:20:2:2
# PROBLEMS
**Parse Error**
at 2:3 to 2:7

**Parse Error**
at 2:13 to 2:13

**Unsupported Node**
at 1:20 to 2:2

**Unsupported Node**
at 2:13 to 2:13

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
(expr :tag block :type "Num(_size)")
~~~
# TYPES
~~~roc
~~~
