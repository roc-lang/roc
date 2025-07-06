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
PARSE ERROR - ann_closed_union.md:2:26:2:32
UNEXPECTED TOKEN IN EXPRESSION - ann_closed_union.md:2:31:2:31
UNUSED VARIABLE - ann_closed_union.md:3:5:3:8
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:10),OpColon(2:11-2:12),OpenSquare(2:13-2:14),UpperIdent(2:14-2:19),Comma(2:19-2:20),UpperIdent(2:21-2:28),NoSpaceOpenRound(2:28-2:29),UpperIdent(2:29-2:33),CloseRound(2:33-2:34),CloseSquare(2:34-2:35),Newline(1:1-1:1),
LowerIdent(3:5-3:10),OpAssign(3:11-3:12),UpperIdent(3:13-3:18),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:5-5:10),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-3.10 (name "apple")
			(ty-tag-union @2.13-2.35
				(tags
					(ty @2.14-2.19 (name "Apple"))
					(ty-apply @2.21-2.34
						(ty @2.21-2.28 (name "IsFruit"))
						(ty @2.29-2.33 (name "Bool"))))))
		(s-decl @3.5-3.18
			(p-ident @3.5-3.10 (raw "apple"))
			(e-tag @3.13-3.18 (raw "Apple")))
		(e-ident @5.5-5.10 (raw "apple"))))
~~~
# FORMATTED
~~~roc
{
	apple : [Apple, IsFruit(Bool)]
	apple = Apple

	apple
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.5-3.10 (name "apple")
		(ty-tag-union @2.13-2.35
			(ty @2.14-2.19 (name "Apple"))
			(ty-apply @2.21-2.34 (symbol "IsFruit")
				(ty @2.29-2.33 (name "Bool")))))
	(s-let @3.5-3.18
		(p-assign @3.5-3.10 (ident "apple"))
		(e-tag @3.13-3.18 (name "Apple")))
	(e-lookup-local @5.5-5.10
		(pattern @3.5-3.10)))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "[Apple]*"))
~~~
