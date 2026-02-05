# META
~~~ini
description=Exposed item name conflicts with local definitions
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [parse]

# Local definition with same name as exposed item
parse = 42

main = {
    result = parse
    result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json")
			(exposing
				(exposed-lower-ident
					(text "parse"))))
		(s-decl
			(p-ident (raw "parse"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "result"))
						(e-ident (raw "parse")))
					(e-ident (raw "result")))))))
~~~
# FORMATTED
~~~roc
import json.Json exposing [parse]

# Local definition with same name as exposed item
parse = 42

main = {
	result = parse
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "result"))
				(e-lookup-local
					(p-assign (ident "parse"))))
			(e-lookup-local
				(p-assign (ident "result")))))
	(s-import (module "json.Json")
		(exposes
			(exposed (name "parse") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
