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
~~~
# FORMATTED
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
	combined = Str.concat(
		
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
Nothing is named **Http.get** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_comprehensive.md:8:14:8:22:**
```roc
    client = Http.get
```
             ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.utf8** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_comprehensive.md:9:14:9:23:**
```roc
    parser = Json.utf8
```
             ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.parse** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_comprehensive.md:13:15:13:25:**
```roc
    result1 = Json.parse
```
              ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Http.post** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_comprehensive.md:16:15:16:24:**
```roc
    result2 = Http.post
```
              ^^^^^^^^^


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
        (Expr.module_access
          (Expr.lookup "Http")
          (Expr.record_accessor)
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "parser"))
        (Expr.module_access
          (Expr.lookup "Json")
          (Expr.record_accessor)
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "helper"))
        (Expr.module_access
          (Expr.lookup "Str")
          (Expr.record_accessor)
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "result1"))
        (Expr.module_access
          (Expr.lookup "Json")
          (Expr.record_accessor)
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "result2"))
        (Expr.module_access
          (Expr.lookup "Http")
          (Expr.record_accessor)
        )
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
        (Expr.apply_ident)
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
