# META
~~~ini
description=the int test platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {
        multiplyInts : I64, I64 -> I64
    }
    exposes []
    packages {}
    provides { "roc_multiplyInts": multiplyInts }

multiplyInts : I64, I64 -> I64
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_int.md:7:16:7:48
DECLARATION HAS NO VALUE - platform_int.md:9:1:9:31
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 7 16) (end 7 48))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "multiplyInts")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform_int.md") (start 7 16) (end 7 48) (annotation error) (line-text "    provides { \"roc_multiplyInts\": multiplyInts }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "multiplyInts")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 9 1) (end 9 31))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "platform_int.md") (start 9 1) (end 9 31) (annotation error) (line-text "multiplyInts : I64, I64 -> I64"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires
			(requires-entry
				(type-aliases)
				(entrypoint "multiplyInts")
				(ty-fn
					(ty (name "I64"))
					(ty (name "I64"))
					(ty (name "I64")))))
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_multiplyInts") (func "multiplyInts"))))
	(statements
		(s-type-anno (name "multiplyInts")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))
				(ty (name "I64"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {
		multiplyInts : I64, I64 -> I64
	}
	exposes []
	packages {}
	provides { "roc_multiplyInts": multiplyInts }

multiplyInts : I64, I64 -> I64
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "multiplyInts"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64, I64 -> I64")))
	(expressions
		(expr (type "I64, I64 -> I64"))))
~~~
