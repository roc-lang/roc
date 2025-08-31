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
	result1 = Json.parse
	result2 = Http.post
	result3 = get
	result4 = post
	combined = Str.concat(
		(client, parser, helper, result1, result2, result3, result4, combined),
	)

}

# Test direct module access
# Test aliased module access
# Test exposed items (should work without module prefix)
# Test multiple qualified access
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:3:1:3:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:4:1:4:48:**
```roc
import http.Client as Http exposing [get, post]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:5:1:5:27:**
```roc
import utils.String as Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:8:14:8:18:**
```roc
    client = Http.get
```
             ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:9:14:9:18:**
```roc
    parser = Json.utf8
```
             ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:10:14:10:17:**
```roc
    helper = Str.trim
```
             ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:13:15:13:19:**
```roc
    result1 = Json.parse
```
              ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:16:15:16:19:**
```roc
    result2 = Http.post
```
              ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_comprehensive.md:23:16:23:19:**
```roc
    combined = Str.concat
```
               ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.block
      (Expr.binop_equals
        (Expr.lookup "client")
        (Expr.lambda)
      )
      (Expr.binop_equals
        (Expr.lookup "parser")
        (Expr.lambda)
      )
      (Expr.binop_equals
        (Expr.lookup "helper")
        (Expr.lambda)
      )
      (Expr.binop_equals
        (Expr.lookup "result1")
        (Expr.lambda)
      )
      (Expr.binop_equals
        (Expr.lookup "result2")
        (Expr.lambda)
      )
      (Expr.binop_equals
        (Expr.lookup "result3")
        (Expr.lookup "get")
      )
      (Expr.binop_equals
        (Expr.lookup "result4")
        (Expr.lookup "post")
      )
      (Expr.binop_equals
        (Expr.lookup "combined")
        (Expr.apply_ident)
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
main : _a
~~~
