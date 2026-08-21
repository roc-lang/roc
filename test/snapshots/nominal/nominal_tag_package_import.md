# META
~~~ini
description=Example of a nominal tag union import from a package
type=snippet
~~~
# SOURCE
~~~roc
# import the Color mod from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color mod
blue : CC.Color
blue = CC.Color.RGB(0,0,255)
~~~
# EXPECTED
MOD NOT FOUND - nominal_tag_package_import.md:5:10:5:16
MOD NOT FOUND - nominal_tag_package_import.md:6:10:6:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 5 10) (end 5 16))
		(headline
			(text "This ")
			(annotated code "Color")
			(reflow " type is declared to be in ")
			(annotated code "styles.Color")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_tag_package_import.md") (start 5 10) (end 5 16) (annotation error) (line-text "blue : CC.Color"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 6 10) (end 6 16))
		(headline
			(text "This ")
			(annotated code "Color")
			(reflow " type is declared to be in ")
			(annotated code "styles.Color")
			(reflow ", which does not exist."))
		(document
			(source-region (file "nominal_tag_package_import.md") (start 6 10) (end 6 16) (annotation error) (line-text "blue = CC.Color.RGB(0,0,255)")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,Comma,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "styles.Color") (alias "CC"))
		(s-type-anno (name "blue")
			(ty (name "CC.Color")))
		(s-decl
			(p-ident (raw "blue"))
			(e-apply
				(e-tag (raw "CC.Color.RGB"))
				(e-int (raw "0"))
				(e-int (raw "0"))
				(e-int (raw "255"))))))
~~~
# FORMATTED
~~~roc
# import the Color mod from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color mod
blue : CC.Color
blue = CC.Color.RGB(0, 0, 255)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "blue"))
		(e-runtime-error (tag "type_from_missing_mod"))
		(annotation
			(ty-malformed)))
	(s-import (mod "styles.Color")
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
