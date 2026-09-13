# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform"
requires{}{n:0[import S	exposing[
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_061.md:1:9:1:10
UNEXPECTED TYPE SYNTAX - fuzz_crash_061.md:2:14:2:15
EXPECTED CLOSING BRACE - fuzz_crash_061.md:1:1:1:9
EXPECTED CLOSING BRACKET - fuzz_crash_061.md:2:16:2:22
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 1 9) (end 1 10))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_061.md") (start 1 9) (end 1 10) (annotation error) (line-text "platform\""))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 2 14) (end 2 15))
		(headline
			(reflow "I was parsing a type annotation, and this token cannot start a type here."))
		(document
			(reflow "Types can be type variables, uppercase type names, function types, tuples, records, or tag unions.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_061.md") (start 2 14) (end 2 15) (annotation error) (line-text "requires{}{n:0[import S\texposing["))))
	(report
		(severity runtime_error)
		(title "Expected Closing Brace")
		(region (start 1 1) (end 1 9))
		(headline
			(reflow "I was parsing a `requires` section, and I expected a closing `}`."))
		(document
			(reflow "Close the requires record after the final entrypoint signature.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "requires { main : {} => I32 }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "platform")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_061.md") (start 1 1) (end 1 9) (annotation error) (line-text "platform\""))))
	(report
		(severity runtime_error)
		(title "Expected Closing Bracket")
		(region (start 2 16) (end 2 22))
		(headline
			(reflow "I was parsing an import exposing clause, and I expected a closing `]`."))
		(document
			(reflow "Close the exposing list after the final imported name.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json exposing [decode, encode]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_061.md") (start 2 16) (end 2 22) (annotation error) (line-text "requires{}{n:0[import S\texposing[")))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,OpenCurly,LowerIdent,OpColon,Int,OpenSquare,KwImport,UpperIdent,KwExposing,OpenSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_requires_signatures_close_curly"))
	(statements
		(s-malformed (tag "import_exposing_no_close"))))
~~~
# FORMATTED
~~~roc
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
