# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:||{0}}}}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),OpenCurly(1:10-1:11),LowerIdent(1:11-1:12),OpColon(1:12-1:13),OpenCurly(1:13-1:14),OpenCurly(1:14-1:15),LowerIdent(1:15-1:16),OpColon(1:16-1:17),OpBar(1:17-1:18),OpBar(1:18-1:19),OpenCurly(1:19-1:20),Int(1:20-1:21),CloseCurly(1:21-1:22),CloseCurly(1:22-1:23),CloseCurly(1:23-1:24),CloseCurly(1:24-1:25),EndOfFile(1:25-1:25),
~~~
# PARSE
~~~clojure
(file @1.1-1.25
	(package @1.1-1.25
		(exposes @1.8-1.10)
		(packages @1.10-1.25
			(record-field @1.11-1.24 (name "d")
				(e-block @1.13-1.24
					(statements
						(e-record @1.14-1.23
							(field (field "d")
								(e-lambda @1.17-1.22
									(args)
									(e-block @1.19-1.22
										(statements
											(e-int @1.20-1.21 (raw "0"))))))))))))
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
				},
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
