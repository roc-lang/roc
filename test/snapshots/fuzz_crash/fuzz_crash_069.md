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
KwPackage(1:1-1:8),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),OpenCurly(1:10-1:11),LowerIdent(1:11-1:12),OpColon(1:12-1:13),OpenCurly(1:13-1:14),Int(1:14-1:15),CloseCurly(1:15-1:16),CloseCurly(1:16-1:17),
EndOfFile(2:1-2:1),
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
(can-ir
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions))
~~~
