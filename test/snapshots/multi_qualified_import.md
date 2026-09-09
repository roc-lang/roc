# META
~~~ini
description=Test multi-level qualified imports and type annotations
type=snippet
~~~
# SOURCE
~~~roc
import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json.Core.Utf8.encode("hello")
~~~
# EXPECTED
EXPECTED RECORD ACCESSOR - multi_qualified_import.md:12:12:12:17
EXPECTED RECORD ACCESSOR - multi_qualified_import.md:12:17:12:22
MOD NOT FOUND - multi_qualified_import.md:3:16:3:23
DOES NOT EXIST - multi_qualified_import.md:4:16:4:45
MOD NOT IMPORTED - multi_qualified_import.md:7:11:7:33
UNUSED VARIABLE - multi_qualified_import.md:8:12:8:19
MOD NOT IMPORTED - multi_qualified_import.md:11:8:11:34
UNRECOGNIZED SYNTAX - multi_qualified_import.md:12:8:12:38
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 12 12) (end 12 17))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".Core")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "multi_qualified_import.md") (start 12 12) (end 12 17) (annotation error) (line-text "data = json.Core.Utf8.encode(\"hello\")"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 12 17) (end 12 22))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".Utf8")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "multi_qualified_import.md") (start 12 17) (end 12 22) (annotation error) (line-text "data = json.Core.Utf8.encode(\"hello\")"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 3 16) (end 3 23))
		(headline
			(text "This ")
			(annotated code "Encoder")
			(reflow " type is declared to be in ")
			(annotated code "json.Core")
			(reflow ", which does not exist."))
		(document
			(source-region (file "multi_qualified_import.md") (start 3 16) (end 3 23) (annotation error) (line-text "json_encoder : Encoder"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 4 16) (end 4 45))
		(headline
			(annotated code "Json.defaultEncoder")
			(reflow " does not exist."))
		(document
			(annotated code "Json")
			(reflow " is in scope, but it has no associated ")
			(annotated code "defaultEncoder")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "multi_qualified_import.md") (start 4 16) (end 4 45) (annotation error) (line-text "json_encoder = Json.Core.Utf8.defaultEncoder"))))
	(report
		(severity runtime_error)
		(title "Mod Not Imported")
		(region (start 7 11) (end 7 33))
		(headline
			(text "There is no mod with the name ")
			(annotated code "json.Core.Utf8")
			(reflow " imported into this Roc file."))
		(document
			(source-region (file "multi_qualified_import.md") (start 7 11) (end 7 33) (annotation error) (line-text "process : json.Core.Utf8.Encoder -> Str"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 8 12) (end 8 19))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "encoder")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_encoder")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "multi_qualified_import.md") (start 8 12) (end 8 19) (annotation error) (line-text "process = |encoder| \"processing\""))))
	(report
		(severity runtime_error)
		(title "Mod Not Imported")
		(region (start 11 8) (end 11 34))
		(headline
			(text "There is no mod with the name ")
			(annotated code "json.Core.Utf8")
			(reflow " imported into this Roc file."))
		(document
			(source-region (file "multi_qualified_import.md") (start 11 8) (end 11 34) (annotation error) (line-text "data : json.Core.Utf8.EncodedData"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 12 8) (end 12 38))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "multi_qualified_import.md") (start 12 8) (end 12 38) (annotation error) (line-text "data = json.Core.Utf8.encode(\"hello\")"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Core.Utf8")
			(exposing
				(exposed-upper-ident (text "Encoder"))))
		(s-type-anno (name "json_encoder")
			(ty (name "Encoder")))
		(s-decl
			(p-ident (raw "json_encoder"))
			(e-ident (raw "Json.Core.Utf8.defaultEncoder")))
		(s-type-anno (name "process")
			(ty-fn
				(ty (name "json.Core.Utf8.Encoder"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "encoder")))
				(e-string
					(e-string-part (raw "processing")))))
		(s-type-anno (name "data")
			(ty (name "json.Core.Utf8.EncodedData")))
		(s-decl
			(p-ident (raw "data"))
			(e-method-call (method ".encode")
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(args
					(e-string
						(e-string-part (raw "hello"))))))))
~~~
# FORMATTED
~~~roc
import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = .encode("hello")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "json_encoder"))
		(e-runtime-error (tag "nested_value_not_found"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "process"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "data"))
		(e-runtime-error (tag "expr_not_canonicalized"))
		(annotation
			(ty-malformed)))
	(s-import (mod "json.Core")
		(exposes
			(exposed (name "Utf8") (alias "Utf8") (wildcard false))
			(exposed (name "Utf8.Encoder") (alias "Encoder") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error -> Str"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error -> Str"))
		(expr (type "Error"))))
~~~
