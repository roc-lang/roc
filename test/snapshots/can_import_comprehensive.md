# META
~~~ini
description=Comprehensive import test with various module access patterns
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
    client = Http.get
    parser = Json.utf8
    helper = Str.trim

    # Test direct module access
    result1 = Json.parse

    # Test aliased module access
    result2 = Http.post

    # Test exposed items (should work without module prefix)
    result3 = get
    result4 = post

    # Test multiple qualified access
    combined = Str.concat

    (
        client,
        parser,
        helper,
        result1,
        result2,
        result3,
        result4,
        combined,
    )
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare LowerIdent Comma LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_dot
      (lc "json")
      (uc "Json")
    )
  )
  (import
    (binop_exposing
      (binop_as
        (binop_dot
          (lc "http")
          (uc "Client")
        )
        (uc "Http")
      )
      (list_literal
        (lc "get")
        (lc "post")
      )
    )
  )
  (import
    (binop_as
      (binop_dot
        (lc "utils")
        (uc "String")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "client")
        (binop_dot
          (uc "Http")
          (dot_lc "get")
        )
      )
      (binop_equals
        (lc "parser")
        (binop_dot
          (uc "Json")
          (dot_lc "utf8")
        )
      )
      (binop_equals
        (lc "helper")
        (binop_dot
          (uc "Str")
          (dot_lc "trim")
        )
      )
      (binop_equals
        (lc "result1")
        (binop_dot
          (uc "Json")
          (dot_lc "parse")
        )
      )
      (binop_equals
        (lc "result2")
        (binop_dot
          (uc "Http")
          (dot_lc "post")
        )
      )
      (binop_equals
        (lc "result3")
        (lc "get")
      )
      (binop_equals
        (lc "result4")
        (lc "post")
      )
      (binop_equals
        (lc "combined")
        (apply_anon
          (binop_dot
            (uc "Str")
            (dot_lc "concat")
          )
          (tuple_literal
            (lc "client")
            (lc "parser")
            (lc "helper")
            (lc "result1")
            (lc "result2")
            (lc "result3")
            (lc "result4")
            (lc "combined")
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json
import (http.Client) as Http exposing [get, post]
import utils.String as Str
main = {
	client = (Http..get)
	parser = (Json..utf8)
	helper = (Str..trim)
	# Test direct module access
	result1 = (Json..parse)
	# Test aliased module access
	result2 = (Http..post)
	# Test exposed items (should work without module prefix)
	result3 = get
	result4 = post
	# Test multiple qualified access
	combined = Str..concat(
		(client, parser, helper, result1, result2, result3, result4, combined),
	)

}
~~~
# EXPECTED
MODULE NOT FOUND - can_import_comprehensive.md:3:1:3:17
MODULE NOT FOUND - can_import_comprehensive.md:4:1:4:48
MODULE NOT FOUND - can_import_comprehensive.md:5:1:5:27
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**can_import_comprehensive.md:23:26:25:5:**
```roc
    combined = Str.concat

    (
```


**UNDEFINED VARIABLE**
Nothing is named **get** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_comprehensive.md:19:15:19:18:**
```roc
    result3 = get
```
              ^^^


**UNDEFINED VARIABLE**
Nothing is named **post** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_comprehensive.md:20:15:20:19:**
```roc
    result4 = post
```
              ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "client"))
        (Expr.record_access)
      )
      (Stmt.assign
        (pattern (Patt.ident "parser"))
        (Expr.record_access)
      )
      (Stmt.assign
        (pattern (Patt.ident "helper"))
        (Expr.record_access)
      )
      (Stmt.assign
        (pattern (Patt.ident "result1"))
        (Expr.record_access)
      )
      (Stmt.assign
        (pattern (Patt.ident "result2"))
        (Expr.record_access)
      )
      (Stmt.assign
        (pattern (Patt.ident "result3"))
        (Expr.lookup "get")
      )
      (Stmt.assign
        (pattern (Patt.ident "result4"))
        (Expr.lookup "post")
      )
      (Stmt.assign
        (pattern (Patt.ident "combined"))
        (Expr.fn_call)
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 73
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
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 -> #68)
(var #22 -> #25)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 -> #30)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 -> #35)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 -> #40)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 -> #45)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 -> #48)
(var #48 _)
(var #49 _)
(var #50 -> #51)
(var #51 _)
(var #52 _)
(var #53 -> #66)
(var #54 _)
(var #55 _)
(var #56 -> #72)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 -> #71)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 tuple)
(var #72 fn_pure)
~~~
# TYPES
~~~roc
helper : _a
result4 : _a
combined : _a
main : _a
client : _a
result1 : _a
parser : _a
result2 : _a
result3 : _a
~~~
