# META
~~~ini
description=platform_header_empty (1)
type=file
~~~
# SOURCE
~~~roc
platform "foo"
	requires {} {}
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
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:14),StringEnd(1:14-1:15),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),CloseCurly(2:12-2:13),OpenCurly(2:14-2:15),CloseCurly(2:15-2:16),
KwExposes(3:2-3:9),OpenSquare(3:10-3:11),CloseSquare(3:11-3:12),
KwPackages(4:2-4:10),OpenCurly(4:11-4:12),CloseCurly(4:12-4:13),
KwProvides(5:2-5:10),OpenCurly(5:11-5:12),CloseCurly(5:12-5:13),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.13
	(platform @1.1-5.13 (name "foo")
		(rigids @2.11-2.13)
		(ty-record @2.14-2.16)
		(exposes @3.10-3.12)
		(packages @4.11-4.13)
		(provides @5.11-5.13))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
