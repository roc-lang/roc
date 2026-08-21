# META
~~~ini
description=the str test platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {
        processString : Str -> Str
    }
    exposes []
    packages {}
    provides { "roc_processString": processString }

processString : Str -> Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_str.md:7:16:7:50
DECLARATION HAS NO VALUE - platform_str.md:9:1:9:27
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 7 16) (end 7 50))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "processString")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform_str.md") (start 7 16) (end 7 50) (annotation error) (line-text "    provides { \"roc_processString\": processString }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "processString")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 9 1) (end 9 27))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "platform_str.md") (start 9 1) (end 9 27) (annotation error) (line-text "processString : Str -> Str"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires
			(requires-entry
				(type-aliases)
				(entrypoint "processString")
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))))
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_processString") (func "processString"))))
	(statements
		(s-type-anno (name "processString")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {
		processString : Str -> Str
	}
	exposes []
	packages {}
	provides { "roc_processString": processString }

processString : Str -> Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processString"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(expressions
		(expr (type "Str -> Str"))))
~~~
