# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:{0}.c}}}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,OpenCurly,OpenCurly,LowerIdent,OpColon,OpenCurly,Int,CloseCurly,NoSpaceDotLowerIdent,CloseCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes)
		(packages
			(record-field (name "d")
				(e-block
					(statements
						(e-record
							(field (field "d")
								(e-field-access
									(e-block
										(statements
											(e-int (raw "0"))))
									(e-ident (raw "c"))))))))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		d: {
			{
				d: {
					0
				}.c,
			}
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
