# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:if 0 0 else{{}}}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwIf,Int,Int,KwElse,OpenCurly,OpenCurly,CloseCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes)
		(packages
			(record-field (name "d")
				(e-if-then-else
					(e-int (raw "0"))
					(e-int (raw "0"))
					(e-block
						(statements
							(e-record)))))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		d: if 0 0 else {
			{}
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
