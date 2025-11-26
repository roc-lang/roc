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
NIL
# PROBLEMS
NIL
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
