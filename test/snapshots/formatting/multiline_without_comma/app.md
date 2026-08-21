# META
~~~ini
description=Multiline without comma formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!
] {
	pf: platform "../basic-cli/main.roc",
	a: "a"
}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - app.md:3:2:3:5
EXPOSED BUT NOT DEFINED - app.md:2:2:2:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 3 2) (end 3 5))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "a2!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "app.md") (start 3 2) (end 3 5) (annotation error) (line-text "\ta2!"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "a2!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 2 2) (end 2 5))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "a1!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "app.md") (start 2 2) (end 2 5) (annotation error) (line-text "\ta1!,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "a1!")
			(reflow " in this mod, or by removing it from the list of exposed values."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,OpenCurly,
LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "a1!"))
			(exposed-lower-ident
				(text "a2!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))
			(record-field (name "a")
				(e-string
					(e-string-part (raw "a"))))))
	(statements))
~~~
# FORMATTED
~~~roc
app [a1!, a2!] { pf: platform "../basic-cli/main.roc", a: "a" }
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
