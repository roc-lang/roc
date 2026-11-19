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
RESERVED TYPE NAME - can_import_type_alias_conflict.md:1:12:1:17
TYPE REDECLARED - can_import_type_alias_conflict.md:4:1:4:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Reserved Type Name")
		(region (start 1 12) (end 1 17))
		(headline
			(text "This import cannot bind ")
			(annotated symbol-unqualified "Json")
			(text " because that name is reserved for a builtin type."))
		(document
			(reflow "Use `as` to give the imported type a different name.")
			(line-break)
			(source-region (file "can_import_type_alias_conflict.md") (start 1 12) (end 1 17) (annotation error) (line-text "import json.Json exposing [JsonValue]"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 4 1) (end 4 16))
		(headline
			(reflow "The type ")
			(annotated code "JsonValue")
			(reflow " is being redeclared."))
		(document
			(source-region (file "can_import_type_alias_conflict.md") (start 4 1) (end 4 16) (annotation error) (line-text "JsonValue : U64"))
			(line-break)
			(reflow "But ")
			(annotated type "JsonValue")
			(reflow " was already declared in ")
			(source-location
				(file "can_import_type_alias_conflict.md")
				(line 1)
				(column 1))
			(reflow ".")
			(line-break)
			(source-region (file "can_import_type_alias_conflict.md") (start 1 1) (end 1 38) (annotation dim) (line-text "import json.Json exposing [JsonValue]")))))
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
