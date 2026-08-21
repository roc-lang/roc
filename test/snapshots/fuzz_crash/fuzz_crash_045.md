# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform""requires{}{}exposes[]packages{}provides[
~~~
# EXPECTED
EXPECTED OPENING BRACE - fuzz_crash_045.md:1:50:1:51
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Opening Brace")
		(region (start 1 50) (end 1 51))
		(headline
			(reflow "I was parsing a `provides` section, and I expected an opening `{`."))
		(document
			(reflow "Host symbol mappings are written as record-like entries inside braces.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "provides { \"roc_main\": main }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_045.md") (start 1 50) (end 1 51) (annotation error) (line-text "platform\"\"requires{}{}exposes[]packages{}provides[")))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,CloseCurly,OpenCurly,CloseCurly,KwExposes,OpenSquare,CloseSquare,KwPackages,OpenCurly,CloseCurly,KwProvides,OpenSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires)
		(exposes)
		(packages)
		(provides))
	(statements))
~~~
# FORMATTED
~~~roc
platform ""
	requires {}
	exposes []
	packages {}
	provides {}
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
