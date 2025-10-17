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
MODULE NOT FOUND - fuzz_crash_054.md:1:20:2:3
# PROBLEMS
**MODULE NOT FOUND**
The module `S` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_054.md:1:20:2:3:**
```roc
app[]{f:platform""}import S exposing[c as
f]
```


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
	(s-import (module "S")
		(exposes
			(exposed (name "c") (alias "f") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
