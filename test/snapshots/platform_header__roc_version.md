# META
~~~ini
description=Platform Header - pinned roc version
type=header
~~~
# SOURCE
~~~roc
platform "test-platform"
	requires {}
	exposes []
	packages { roc: "nightly-2026-July-31-123c5d7" }
	provides {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
KwProvides,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(platform (name "test-platform") (roc-version "nightly-2026-July-31-123c5d7")
	(requires)
	(exposes)
	(packages
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "nightly-2026-July-31-123c5d7")))))
	(provides))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
