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
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare LowerIdent Comma LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma CloseRound CloseCurly ~~~
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
		(
			client,
			parser,
			helper,
			result1,
			result2,
			result3,
			result4,
			combined,
		),
	)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 34:5 to 35:1

**Parse Error**
at 23:16 to 35:1

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 4:1 to 4:48

**Unsupported Node**
at 5:1 to 5:27

**Unsupported Node**
at 8:14 to 8:18

**Unsupported Node**
at 9:14 to 9:18

**Unsupported Node**
at 10:14 to 10:17

**Unsupported Node**
at 13:15 to 13:19

**Unsupported Node**
at 16:15 to 16:19

**Unsupported Node**
at 23:16 to 23:19

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
