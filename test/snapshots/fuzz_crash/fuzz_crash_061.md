# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform"
requires{}{n:0[import S	exposing[
~~~
# TOKENS
~~~text
KwPlatform MalformedString KwRequires OpenCurly CloseCurly OpenCurly LowerIdent OpColon Int OpenSquare KwImport UpperIdent KwExposing OpenSquare ~~~
# PARSE
~~~clojure
(block
  (list_literal)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_061.md:2:14:2:15
PARSE ERROR - fuzz_crash_061.md:2:11:2:12
PARSE ERROR - fuzz_crash_061.md:2:16:2:22
# PROBLEMS
**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 1:1 to 2:15

**Expected Exposes**
at 1:1 to 2:15

**Parse Error**
at 2:16 to 2:16

**Expected Close Square Bracket**
at 1:1 to 2:23

**Expected Packages**
at 1:1 to 2:23

**Parse Error**
at 1:1 to 2:23

**Parse Error**
at 2:23 to 2:23

**Expected Close Curly Brace**
at 1:1 to 2:25

**Parse Error**
at 1:1 to 2:25

**Parse Error**
at 1:1 to 2:25

**Parse Error**
at 2:25 to 2:25

**Parse Error**
at 1:1 to 2:33

**Parse Error**
at 2:33 to 2:34

**Unsupported Node**
at 2:33 to 2:34

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
