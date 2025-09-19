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
KwPackage(1:1-1:8),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),OpenCurly(1:10-1:11),LowerIdent(1:11-1:12),OpColon(1:12-1:13),OpenCurly(1:13-1:14),OpenCurly(1:14-1:15),LowerIdent(1:15-1:16),OpColon(1:16-1:17),OpenCurly(1:17-1:18),Int(1:18-1:19),CloseCurly(1:19-1:20),NoSpaceDotLowerIdent(1:20-1:22),CloseCurly(1:22-1:23),CloseCurly(1:23-1:24),CloseCurly(1:24-1:25),
EndOfFile(2:1-2:1),
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
								(e-field-access @1.17-1.22
									(e-block @1.17-1.20
										(statements
											(e-int @1.18-1.19 (raw "0"))))
									(e-ident @1.20-1.22 (raw "c"))))))))))
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
