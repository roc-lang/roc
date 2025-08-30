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
(platform-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
platform "
 requires n : 0 exposes  [import ]

[]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 2:1

**Parse Error**
at 1:1 to 2:15

**Expected Exposes**
at 1:1 to 2:15

**Parse Error**
at 2:16 to 2:23

**Expected Close Square Bracket**
at 1:1 to 2:23

**Expected Packages**
at 1:1 to 2:23

**Parse Error**
at 1:1 to 2:23

**Parse Error**
at 2:23 to 2:25

**Expected Close Curly Brace**
at 1:1 to 2:25

**Parse Error**
at 1:1 to 2:25

**Parse Error**
at 1:1 to 2:25

**Parse Error**
at 2:25 to 2:33

**Parse Error**
at 1:1 to 2:33

**Parse Error**
at 2:33 to 2:34

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
