# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import S exposing[c as
f]
~~~
# EXPECTED
MOD NOT FOUND - fuzz_crash_054.md:1:20:2:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 20) (end 2 3))
		(headline
			(text "The mod ")
			(annotated code "S")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_054.md") (start 1 20) (end 2 3) (annotation error) (line-text "app[]{f:platform\"\"}import S exposing[c as\nf]")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,UpperIdent,KwExposing,OpenSquare,LowerIdent,KwAs,
LowerIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
		(s-import (raw "S")
			(exposing
				(exposed-lower-ident
					(text "c")
					(as "f"))))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
import S exposing [
	c as f,
]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "S")
		(exposes
			(exposed (name "c") (alias "f") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
