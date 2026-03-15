# META
~~~ini
description=Imported type alias conflicts with local type
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [JsonValue]

# Local type with same name as exposed type
JsonValue : U64

main = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json")
			(exposing
				(exposed-upper-ident (text "JsonValue"))))
		(s-type-decl
			(header (name "JsonValue")
				(args))
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "main"))
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-num (value "42")))
	(s-alias-decl
		(ty-header (name "JsonValue"))
		(ty-lookup (name "U64") (builtin)))
	(s-import (module "json.Json")
		(exposes
			(exposed (name "JsonValue") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(alias (type "JsonValue")
			(ty-header (name "JsonValue"))))
	(expressions
		(expr (type "Dec"))))
~~~
