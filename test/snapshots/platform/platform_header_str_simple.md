# META
~~~ini
description=Simple plaform module
type=file
~~~
# SOURCE
~~~roc
platform ""
	requires {} { main : Str -> Str }
	exposes []
	packages {}
	provides [entrypoint]

entrypoint : Str -> Str
entrypoint = main
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly CloseCurly OpenCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent CloseCurly KwExposes OpenSquare CloseSquare KwPackages OpenCurly CloseCurly KwProvides OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign LowerIdent ~~~
# PARSE
~~~clojure
(platform-header)
~~~
# FORMATTED
~~~roc
platform "" requires { main : Str -> Str } exposes []


entrypoint : Str -> Str
entrypoint = main
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **main** in this scope.
Is there an **import** or **exposing** missing up-top?

**platform_header_str_simple.md:8:14:8:18:**
```roc
entrypoint = main
```
             ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
