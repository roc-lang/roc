# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:||{0}->R}}}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,OpenCurly,OpenCurly,LowerIdent,OpColon,OpBar,OpBar,OpenCurly,Int,CloseCurly,OpArrow,UpperIdent,CloseCurly,CloseCurly,CloseCurly,
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
								(e-lambda
									(args)
									(e-local-dispatch
										(e-block
											(statements
												(e-int (raw "0"))))
										(e-tag (raw "R")))))))))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		d: {
			{
				d: || {
					0
				}->R,
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
