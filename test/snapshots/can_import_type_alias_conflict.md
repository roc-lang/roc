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
DUPLICATE DEFINITION - can_import_type_alias_conflict.md:1:1:1:38
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 1 1) (end 1 38))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_type_alias_conflict.md") (start 1 1) (end 1 38) (annotation error) (line-text "import json.Json exposing [JsonValue]"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_type_alias_conflict.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_type_alias_conflict.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json exposing [JsonValue]")))))
~~~
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
	(type-mod)
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
	(s-import (mod "json.Json")
		(exposes
			(exposed (name "JsonValue") (wildcard false))))
	(s-alias-decl
		(ty-header (name "JsonValue"))
		(ty-lookup (name "U64") (builtin))))
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
