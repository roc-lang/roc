# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:if 0 0 else{{}}}
~~~
# EXPECTED
EXPECTED PACKAGE PATH - fuzz_crash_076.md:1:13:1:28
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Package Path")
		(region (start 1 13) (end 1 28))
		(headline
			(reflow "I was parsing a dependency entry, and I expected a string path."))
		(document
			(reflow "Normal package dependencies use a string path after the colon. Use ")
			(annotated code "platform \"...\"")
			(reflow " only for the single platform entry.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "json: \"../json/main.roc\"")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "if 0 0 else{{}}")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_076.md") (start 1 13) (end 1 28) (annotation error) (line-text "package[]{d:if 0 0 else{{}}}")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwIf,Int,Int,KwElse,OpenCurly,OpenCurly,CloseCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes)
		(packages
			(record-field (name "d")
				(e-if-then-else
					(e-int (raw "0"))
					(e-int (raw "0"))
					(e-block
						(statements
							(e-record)))))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		d: if (0) 0 else {
			{}
		},
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
