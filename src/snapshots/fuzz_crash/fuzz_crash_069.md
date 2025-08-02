# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{0}}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),OpenCurly(1:10-1:11),LowerIdent(1:11-1:12),OpColon(1:12-1:13),OpenCurly(1:13-1:14),Int(1:14-1:15),CloseCurly(1:15-1:16),CloseCurly(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(file @1.1-1.17
	(package @1.1-1.17
		(exposes @1.8-1.10)
		(packages @1.10-1.17
			(record-field @1.11-1.16 (name "d")
				(e-block @1.13-1.16
					(statements
						(e-int @1.14-1.15 (raw "0")))))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		d: {
			0
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
