# META
~~~ini
description=platform_header_empty (4)
type=file
~~~
# SOURCE
~~~roc
platform "foo" requires {} {} exposes [] packages {} provides {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:14),StringEnd(1:14-1:15),KwRequires(1:16-1:24),OpenCurly(1:25-1:26),CloseCurly(1:26-1:27),OpenCurly(1:28-1:29),CloseCurly(1:29-1:30),KwExposes(1:31-1:38),OpenSquare(1:39-1:40),CloseSquare(1:40-1:41),KwPackages(1:42-1:50),OpenCurly(1:51-1:52),CloseCurly(1:52-1:53),KwProvides(1:54-1:62),OpenCurly(1:63-1:64),CloseCurly(1:64-1:65),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.65
	(platform @1.1-1.65 (name "foo")
		(rigids @1.25-1.27)
		(ty-record @1.28-1.30)
		(exposes @1.39-1.41)
		(packages @1.51-1.53)
		(provides @1.63-1.65))
	(statements))
~~~
# FORMATTED
~~~roc
platform "foo"
	requires {} {}
	exposes []
	packages {}
	provides {}
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
