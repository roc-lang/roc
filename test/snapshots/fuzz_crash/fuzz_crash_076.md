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
KwPackage(1:1-1:8),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),OpenCurly(1:10-1:11),LowerIdent(1:11-1:12),OpColon(1:12-1:13),KwIf(1:13-1:15),Int(1:16-1:17),Int(1:18-1:19),KwElse(1:20-1:24),OpenCurly(1:24-1:25),OpenCurly(1:25-1:26),CloseCurly(1:26-1:27),CloseCurly(1:27-1:28),CloseCurly(1:28-1:29),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.29
	(package @1.1-1.29
		(exposes @1.8-1.10)
		(packages @1.10-1.29
			(record-field @1.11-1.28 (name "d")
				(e-if-then-else @1.13-1.28
					(e-int @1.16-1.17 (raw "0"))
					(e-int @1.18-1.19 (raw "0"))
					(e-block @1.24-1.28
						(statements
							(e-record @1.25-1.27)))))))
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
