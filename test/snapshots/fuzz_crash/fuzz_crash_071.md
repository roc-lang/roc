# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:{0}?}}}
~~~
# EXPECTED
EXPECTED PACKAGE PATH - fuzz_crash_071.md:1:13:1:23
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Package Path")
		(region (start 1 13) (end 1 23))
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
			(annotated code "{{d:{0}?}}")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_071.md") (start 1 13) (end 1 23) (annotation error) (line-text "package[]{d:{{d:{0}?}}}")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,OpenCurly,OpenCurly,LowerIdent,OpColon,OpenCurly,Int,CloseCurly,NoSpaceOpQuestion,CloseCurly,CloseCurly,CloseCurly,
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
								(e-question-suffix
									(e-block
										(statements
											(e-int (raw "0"))))))))))))
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
				}?,
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
