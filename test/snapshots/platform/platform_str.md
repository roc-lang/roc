# META
~~~ini
description=the str test platform
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {} { processString : Str -> Str }
    exposes []
    packages {}
    provides { processString: "processString" }

processString : Str -> Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(rigids)
		(ty-record
			(anno-record-field (name "processString")
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))))
		(exposes)
		(packages)
		(provides
			(record-field (name "processString")
				(e-string
					(e-string-part (raw "processString"))))))
	(statements
		(s-type-anno (name "processString")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {} { processString : Str -> Str }
	exposes []
	packages {}
	provides { processString: "processString" }

processString : Str -> Str
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
