# META
~~~ini
description=platform_header_empty (5) with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform "foo"
    requires {}
    exposes []
    packages {}
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
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "foo")
		(requires)
		(exposes)
		(packages)
		(provides))
	(statements))
~~~
# FORMATTED
~~~roc
platform "foo"
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
