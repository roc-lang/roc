# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:{0}.c}}}
~~~
# EXPECTED
EXPECTED PACKAGE PATH - fuzz_crash_074.md:1:13:1:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Package Path")
		(region (start 1 13) (end 1 24))
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
			(annotated code "{{d:{0}.c}}")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_074.md") (start 1 13) (end 1 24) (annotation error) (line-text "package[]{d:{{d:{0}.c}}}")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,OpenCurly,OpenCurly,LowerIdent,OpColon,OpenCurly,Int,CloseCurly,NoSpaceDotLowerIdent,CloseCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes)
		(packages
			(record-field (name "d")
				(e-block
					(statements
						(e-record
							(field (field "d")
								(e-field-access
									(receiver
										(e-block
											(statements
												(e-int (raw "0")))))
									(segment (mode "required") (field "c"))))))))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		d: {
			{
				d: {
					0
				}.c,
			}
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
