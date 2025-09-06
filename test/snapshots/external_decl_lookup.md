# META
~~~ini
description=External declaration lookup from json module
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external module!")
    Stdout.line!(result)
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound String CloseRound UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (import
    (binop_pipe
      (lc "pf")
      (uc "Stdout")
    )
  )
  (import
    (binop_pipe
      (lc "json")
      (uc "Json")
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "result")
            (apply_anon
              (binop_pipe
                (uc "Json")
                (dot_lc "utf8")
              )
              (str_literal_big "Hello from external module!")
            )
          )
          (apply_anon
            (binop_pipe
              (uc "Stdout")
              (not_lc "line")
            )
            (lc "result")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout
import json.Json
main! = |_| {
	# This should create an external declaration for json.Json.utf8
	result = Json.utf8("Hello from external module!")
	Stdout.line!(result)
}
~~~
# EXPECTED
MODULE NOT FOUND - external_decl_lookup.md:3:1:3:17
MODULE NOT FOUND - external_decl_lookup.md:4:1:4:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **pf** in this scope.
Is there an **import** or **exposing** missing up-top?

**external_decl_lookup.md:3:8:3:10:**
```roc
import pf.Stdout
```
       ^^


**UNDEFINED VARIABLE**
Nothing is named **json** in this scope.
Is there an **import** or **exposing** missing up-top?

**external_decl_lookup.md:4:8:4:12:**
```roc
import json.Json
```
       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**external_decl_lookup.md:9:11:9:17:**
```roc
    Stdout.line!(result)
```
          ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 37
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
(var #14 _)
(var #15 -> #36)
(var #16 _)
(var #17 -> #22)
(var #18 _)
(var #19 _)
(var #20 -> #34)
(var #21 Str)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #35)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 -> #36)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 fn_pure)
(var #35 fn_pure)
(var #36 fn_pure)
~~~
# TYPES
~~~roc
main : _arg -> _ret
result : _a
~~~
