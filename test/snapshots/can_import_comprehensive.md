# META
~~~ini
description=Comprehensive import test with various mod access patterns
type=snippet
~~~
# SOURCE
~~~roc
import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
    client = Http.get
    parser = Json.utf8
    helper = Str.trim

    # Test direct mod access
    result1 = Json.parse

    # Test aliased mod access
    result2 = Http.post

    # Test exposed items (should work without mod prefix)
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
# EXPECTED
RESERVED TYPE NAME - can_import_comprehensive.md:1:12:1:17
RESERVED TYPE NAME - can_import_comprehensive.md:3:24:3:27
DOES NOT EXIST - can_import_comprehensive.md:6:14:6:22
DOES NOT EXIST - can_import_comprehensive.md:14:15:14:24
DOES NOT EXIST - can_import_comprehensive.md:17:15:17:18
DOES NOT EXIST - can_import_comprehensive.md:18:15:18:19
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
			(source-region (file "can_import_comprehensive.md") (start 1 12) (end 1 17) (annotation error) (line-text "import json.Json"))))
	(report
		(severity runtime_error)
		(title "Reserved Type Name")
		(region (start 3 24) (end 3 27))
		(headline
			(text "This import cannot bind ")
			(annotated symbol-unqualified "Str")
			(text " because that name is reserved for a builtin type."))
		(document
			(reflow "Use `as` to give the imported type a different name.")
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 3 24) (end 3 27) (annotation error) (line-text "import utils.String as Str"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 6 14) (end 6 22))
		(headline
			(annotated symbol-unqualified "Http.get")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_comprehensive.md") (start 6 14) (end 6 22) (annotation error) (line-text "    client = Http.get"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 14 15) (end 14 24))
		(headline
			(annotated symbol-unqualified "Http.post")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_comprehensive.md") (start 14 15) (end 14 24) (annotation error) (line-text "    result2 = Http.post"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 17 15) (end 17 18))
		(headline
			(annotated symbol-unqualified "get")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_comprehensive.md") (start 17 15) (end 17 18) (annotation error) (line-text "    result3 = get"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 18 15) (end 18 19))
		(headline
			(annotated symbol-unqualified "post")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_comprehensive.md") (start 18 15) (end 18 19) (annotation error) (line-text "    result4 = post")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Json"))
		(s-import (raw "http.Client") (alias "Http")
			(exposing
				(exposed-lower-ident
					(text "get"))
				(exposed-lower-ident
					(text "post"))))
		(s-import (raw "utils.String") (alias "Str"))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "client"))
						(e-ident (raw "Http.get")))
					(s-decl
						(p-ident (raw "parser"))
						(e-ident (raw "Json.utf8")))
					(s-decl
						(p-ident (raw "helper"))
						(e-ident (raw "Str.trim")))
					(s-decl
						(p-ident (raw "result1"))
						(e-ident (raw "Json.parse")))
					(s-decl
						(p-ident (raw "result2"))
						(e-ident (raw "Http.post")))
					(s-decl
						(p-ident (raw "result3"))
						(e-ident (raw "get")))
					(s-decl
						(p-ident (raw "result4"))
						(e-ident (raw "post")))
					(s-decl
						(p-ident (raw "combined"))
						(e-ident (raw "Str.concat")))
					(e-tuple
						(e-ident (raw "client"))
						(e-ident (raw "parser"))
						(e-ident (raw "helper"))
						(e-ident (raw "result1"))
						(e-ident (raw "result2"))
						(e-ident (raw "result3"))
						(e-ident (raw "result4"))
						(e-ident (raw "combined"))))))))
~~~
# FORMATTED
~~~roc
import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
	client = Http.get
	parser = Json.utf8
	helper = Str.trim

	# Test direct mod access
	result1 = Json.parse

	# Test aliased mod access
	result2 = Http.post

	# Test exposed items (should work without mod prefix)
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-runtime-error (tag "qualified_ident_does_not_exist"))
			(s-runtime-error (tag "builtin_import_conflict"))
			(s-runtime-error (tag "builtin_import_conflict"))
			(s-runtime-error (tag "builtin_import_conflict"))
			(s-runtime-error (tag "qualified_ident_does_not_exist"))
			(s-runtime-error (tag "qualified_ident_does_not_exist"))
			(s-runtime-error (tag "qualified_ident_does_not_exist"))
			(s-runtime-error (tag "builtin_import_conflict"))
			(e-runtime-error (tag "erroneous_value_expr"))))
	(s-import (mod "json.Json")
		(exposes))
	(s-import (mod "http.Client")
		(exposes
			(exposed (name "get") (wildcard false))
			(exposed (name "post") (wildcard false))))
	(s-import (mod "utils.String")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
