# META
~~~ini
description=Singleline with comma formatting platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires { [R1 : r1, R2 : r2,] for main : R1 -> R2 }
	exposes [E1, E2,]
	packages { pa1: "pa1", pa2: "pa2", }
	provides { "roc_not implemented": pr1, "roc_not implemented": pr2 }
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:5:13:5:39
EXPOSED BUT NOT DEFINED - platform.md:5:41:5:67
EXPOSED BUT NOT DEFINED - platform.md:3:11:3:13
EXPOSED BUT NOT DEFINED - platform.md:3:15:3:17
INVALID HOSTED SECTION - :0:0:0:0
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 5 13) (end 5 39))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "pr1")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 5 13) (end 5 39) (annotation error) (line-text "\tprovides { \"roc_not implemented\": pr1, \"roc_not implemented\": pr2 }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "pr1")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 5 41) (end 5 67))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "pr2")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 5 41) (end 5 67) (annotation error) (line-text "\tprovides { \"roc_not implemented\": pr1, \"roc_not implemented\": pr2 }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "pr2")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 3 11) (end 3 13))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "E1")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 3 11) (end 3 13) (annotation error) (line-text "\texposes [E1, E2,]"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "E1")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 3 15) (end 3 17))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "E2")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 3 15) (end 3 17) (annotation error) (line-text "\texposes [E1, E2,]"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "E2")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Invalid Hosted Section")
		(headline
			(reflow "The platform header uses the linker symbol")
			(reflow " ")
			(annotated code "roc_not implemented")
			(reflow ",")
			(reflow " ")
			(reflow "but linker symbols in platform headers must be valid C identifiers: start with a letter or underscore, followed by only letters, digits, and underscores."))
		(document)))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,LowerIdent,Comma,UpperIdent,OpColon,LowerIdent,Comma,CloseSquare,KwFor,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,
KwExposes,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,CloseSquare,
KwPackages,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "pf")
		(requires
			(requires-entry
				(type-aliases
					(alias (name "R1") (rigid "r1"))
					(alias (name "R2") (rigid "r2")))
				(entrypoint "main")
				(ty-fn
					(ty (name "R1"))
					(ty (name "R2")))))
		(exposes
			(exposed-upper-ident (text "E1"))
			(exposed-upper-ident (text "E2")))
		(packages
			(record-field (name "pa1")
				(e-string
					(e-string-part (raw "pa1"))))
			(record-field (name "pa2")
				(e-string
					(e-string-part (raw "pa2")))))
		(provides
			(symbol-map-entry (symbol "roc_not implemented") (func "pr1"))
			(symbol-map-entry (symbol "roc_not implemented") (func "pr2"))))
	(statements))
~~~
# FORMATTED
~~~roc
platform "pf"
	requires {
		[R1 : r1, R2 : r2] for main : R1 -> R2
	}
	exposes [
		E1,
		E2,
	]
	packages {
		pa1: "pa1",
		pa2: "pa2",
	}
	provides { "roc_not implemented": pr1, "roc_not implemented": pr2 }
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
