# META
~~~ini
description=Singleline with comma formatting app
type=file
~~~
# SOURCE
~~~roc
app [a1!, a2!,] { pf: platform "../basic-cli/main.roc", a: "a", }
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - app.md:1:11:1:14
EXPOSED BUT NOT DEFINED - app.md:1:6:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 1 11) (end 1 14))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "a2!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "app.md") (start 1 11) (end 1 14) (annotation error) (line-text "app [a1!, a2!,] { pf: platform \"../basic-cli/main.roc\", a: \"a\", }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "a2!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 1 6) (end 1 9))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "a1!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "app.md") (start 1 6) (end 1 9) (annotation error) (line-text "app [a1!, a2!,] { pf: platform \"../basic-cli/main.roc\", a: \"a\", }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "a1!")
			(reflow " in this mod, or by removing it from the list of exposed values."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,Comma,LowerIdent,Comma,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
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
app [
	a1!,
	a2!,
] {
	pf: platform "../basic-cli/main.roc",
	a: "a",
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
