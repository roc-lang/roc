# META
~~~ini
description=Example of importing a nominal tag union from another mod
type=snippet
~~~
# SOURCE
~~~roc
import Color

red : Color.RGB
red = Color.RGB.Red
~~~
# EXPECTED
MOD NOT FOUND - nominal_import_type.md:3:12:3:16
MOD NOT FOUND - nominal_import_type.md:4:12:4:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 3 12) (end 3 16))
		(headline
			(text "This ")
			(annotated code "RGB")
			(reflow " type is declared to be in ")
			(annotated code "Color")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_import_type.md") (start 3 12) (end 3 16) (annotation error) (line-text "red : Color.RGB"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 4 12) (end 4 16))
		(headline
			(text "This ")
			(annotated code "RGB")
			(reflow " type is declared to be in ")
			(annotated code "Color")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_import_type.md") (start 4 12) (end 4 16) (annotation error) (line-text "red = Color.RGB.Red")))))
~~~
# TOKENS
~~~zig
KwImport,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "Color"))
		(s-type-anno (name "red")
			(ty (name "Color.RGB")))
		(s-decl
			(p-ident (raw "red"))
			(e-tag (raw "Color.RGB.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "red"))
		(e-runtime-error (tag "type_from_missing_mod"))
		(annotation
			(ty-malformed)))
	(s-import (mod "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
