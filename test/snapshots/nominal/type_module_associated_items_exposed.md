# META
~~~ini
description=Type module with associated items should not report unused variables
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Blah].{
    Bar := {}.{
        baz = {}
    }

    stuff = {}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),CloseSquare(1:13-1:14),Dot(1:14-1:15),OpenCurly(1:15-1:16),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenCurly(2:12-2:13),CloseCurly(2:13-2:14),Dot(2:14-2:15),OpenCurly(2:15-2:16),
LowerIdent(3:9-3:12),OpAssign(3:13-3:14),OpenCurly(3:15-3:16),CloseCurly(3:16-3:17),
CloseCurly(4:5-4:6),
LowerIdent(6:5-6:10),OpAssign(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),
CloseCurly(7:1-7:2),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-7.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.14
				(tags
					(ty @1.9-1.13 (name "Blah"))))
			(associated @1.15-7.2
				(s-type-decl @2.5-4.6
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-record @2.12-2.14)
					(associated @2.15-4.6
						(s-decl @3.9-3.17
							(p-ident @3.9-3.12 (raw "baz"))
							(e-record @3.15-3.17))))
				(s-decl @6.5-6.15
					(p-ident @6.5-6.10 (raw "stuff"))
					(e-record @6.13-6.15))))))
~~~
# FORMATTED
~~~roc
Foo := [Blah].{
	Bar := {}.{
		baz = {}
	}
	stuff = {}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.9-3.17 (ident "Foo.Bar.baz"))
		(e-empty_record @3.15-3.17))
	(d-let
		(p-assign @6.5-6.15 (ident "Foo.stuff"))
		(e-empty_record @6.13-6.15))
	(s-nominal-decl @1.1-7.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.14
			(ty-tag-name @1.9-1.13 (name "Blah"))))
	(s-nominal-decl @2.5-4.6
		(ty-header @2.5-4.6 (name "Foo.Bar"))
		(ty-record @2.12-2.14)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.9-3.17 (type "{}"))
		(patt @6.5-6.15 (type "{}")))
	(type_decls
		(nominal @1.1-7.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-4.6 (type "Foo.Bar")
			(ty-header @2.5-4.6 (name "Foo.Bar"))))
	(expressions
		(expr @3.15-3.17 (type "{}"))
		(expr @6.13-6.15 (type "{}"))))
~~~
