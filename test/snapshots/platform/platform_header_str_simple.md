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
(block
  (binop_colon
    (lc "entrypoint")
    (binop_arrow_call
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "entrypoint")
    (lc "main")
  )
)
~~~
# FORMATTED
~~~roc
platform "" requires { main: Str -> Str } exposes []

entrypoint : Str -> Str
entrypoint = main
~~~
# EXPECTED
UNDEFINED VARIABLE - platform_header_str_simple.md:8:14:8:18
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**platform_header_str_simple.md:7:1:7:11:**
```roc
entrypoint : Str -> Str
```
^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **main** in this scope.
Is there an **import** or **exposing** missing up-top?

**platform_header_str_simple.md:8:14:8:18:**
```roc
entrypoint = main
```
             ^^^^


**SHADOWING**
This definition shadows an existing one.

**platform_header_str_simple.md:8:1:8:11:**
```roc
entrypoint = main
```
^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "entrypoint"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "entrypoint"))
    (Expr.lookup "main")
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 18
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 -> #15)
(var #15 _)
(var #16 _)
(var #17 _)
~~~
# TYPES
~~~roc
entrypoint : _a
~~~
