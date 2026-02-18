# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import fS
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_046.md:1:20:1:26
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_046.md:1:20:1:26:**
```roc
app[]{f:platform""}import fS
```
                   ^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,LowerIdent,UpperIdent,
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
		(s-malformed (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
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
