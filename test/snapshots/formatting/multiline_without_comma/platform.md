# META
~~~ini
description=Multiline without comma formatting platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires {
		[R1 : r1, R2 : r2] for main : R1 -> R2
	}
	exposes [
		E1,
		E2
	]
	packages {
		pa1: "pa1",
		pa2: "pa2"
	}
	provides {
		"roc_not implemented": pr1,
		"roc_not implemented": pr2,
	}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:14:3:14:29
EXPOSED BUT NOT DEFINED - platform.md:15:3:15:29
EXPOSED BUT NOT DEFINED - platform.md:6:3:6:5
EXPOSED BUT NOT DEFINED - platform.md:7:3:7:5
INVALID HOSTED SECTION - :0:0:0:0
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 14 3) (end 14 29))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "pr1")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 14 3) (end 14 29) (annotation error) (line-text "\t\t\"roc_not implemented\": pr1,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "pr1")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 15 3) (end 15 29))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "pr2")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 15 3) (end 15 29) (annotation error) (line-text "\t\t\"roc_not implemented\": pr2,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "pr2")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 6 3) (end 6 5))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "E1")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 6 3) (end 6 5) (annotation error) (line-text "\t\tE1,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "E1")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 7 3) (end 7 5))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "E2")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "platform.md") (start 7 3) (end 7 5) (annotation error) (line-text "\t\tE2"))
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
KwRequires,OpenCurly,
OpenSquare,UpperIdent,OpColon,LowerIdent,Comma,UpperIdent,OpColon,LowerIdent,CloseSquare,KwFor,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,
UpperIdent,Comma,
UpperIdent,
CloseSquare,
KwPackages,OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,
CloseCurly,
KwProvides,OpenCurly,
StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,
StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,
CloseCurly,
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
	exposes [E1, E2]
	packages { pa1: "pa1", pa2: "pa2" }
	provides {
		"roc_not implemented": pr1,
		"roc_not implemented": pr2,
	}
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
