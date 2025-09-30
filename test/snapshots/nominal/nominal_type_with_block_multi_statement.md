# META
~~~ini
description=Nominal type with multi-statement block
type=file
~~~
# SOURCE
~~~roc
module []

Foo := [A, B, C].{
    x = 5
    y = 10
    z = 15
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:4),OpColonEqual(3:5-3:7),OpenSquare(3:8-3:9),UpperIdent(3:9-3:10),Comma(3:10-3:11),UpperIdent(3:12-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:16),CloseSquare(3:16-3:17),Dot(3:17-3:18),OpenCurly(3:18-3:19),
LowerIdent(4:5-4:6),OpAssign(4:7-4:8),Int(4:9-4:10),
LowerIdent(5:5-5:6),OpAssign(5:7-5:8),Int(5:9-5:11),
LowerIdent(6:5-6:6),OpAssign(6:7-6:8),Int(6:9-6:11),
CloseCurly(7:1-7:2),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-7.2
			(header @3.1-3.4 (name "Foo")
				(args))
			(ty-tag-union @3.8-3.17
				(tags
					(ty @3.9-3.10 (name "A"))
					(ty @3.12-3.13 (name "B"))
					(ty @3.15-3.16 (name "C")))))))
~~~
# FORMATTED
~~~roc
module []

Foo := [A, B, C]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @3.1-7.2
		(ty-header @3.1-3.4 (name "Foo"))
		(ty-tag-union @3.8-3.17
			(ty @3.9-3.10 (name "A"))
			(ty @3.12-3.13 (name "B"))
			(ty @3.15-3.16 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @3.1-7.2 (type "Foo")
			(ty-header @3.1-3.4 (name "Foo"))))
	(expressions))
~~~
