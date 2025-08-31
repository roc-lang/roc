# META
~~~ini
description=External declaration lookup from json module
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound String CloseRound UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }


import pf.Stdout
import json.Json
main! = |_| {
	# This should create an external declaration for json.Json.utf8
	result = Json.utf8("Hello from external module!")
	Stdout.line!(result)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Json.utf8** in this scope.
Is there an **import** or **exposing** missing up-top?

**external_decl_lookup.md:8:14:8:23:**
```roc
    result = Json.utf8("Hello from external module!")
```
             ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**external_decl_lookup.md:9:5:9:17:**
```roc
    Stdout.line!(result)
```
    ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
