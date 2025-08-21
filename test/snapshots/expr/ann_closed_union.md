# META
~~~ini
description=Closed Tag Union Type Annotation
type=expr
~~~
# SOURCE
~~~roc
{
	apple : [Apple, IsFruit(Bool)]
	apple = Apple

	apple
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:2-2:7),OpColon(2:8-2:9),OpenSquare(2:10-2:11),UpperIdent(2:11-2:16),Comma(2:16-2:17),UpperIdent(2:18-2:25),NoSpaceOpenRound(2:25-2:26),UpperIdent(2:26-2:30),CloseRound(2:30-2:31),CloseSquare(2:31-2:32),
LowerIdent(3:2-3:7),OpAssign(3:8-3:9),UpperIdent(3:10-3:15),
LowerIdent(5:2-5:7),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.2-2.32 (name "apple")
			(ty-tag-union @2.10-2.32
				(tags
					(ty @2.11-2.16 (name "Apple"))
					(ty-apply @2.18-2.31
						(ty @2.18-2.25 (name "IsFruit"))
						(ty @2.26-2.30 (name "Bool"))))))
		(s-decl @3.2-3.15
			(p-ident @3.2-3.7 (raw "apple"))
			(e-tag @3.10-3.15 (raw "Apple")))
		(e-ident @5.2-5.7 (raw "apple"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.2-2.32 (name "apple")
		(ty-tag-union @2.10-2.32
			(ty @2.11-2.16 (name "Apple"))
			(ty-apply @2.18-2.31 (symbol "IsFruit")
				(ty @2.26-2.30 (name "Bool")))))
	(s-let @3.2-3.15
		(p-assign @3.2-3.7 (ident "apple"))
		(e-tag @3.10-3.15 (name "Apple")))
	(e-lookup-local @5.2-5.7
		(p-assign @3.2-3.7 (ident "apple"))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "[Apple][IsFruit(Bool)]"))
~~~
