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
OpenCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "apple")
			(ty-tag-union
				(tags
					(ty (name "Apple"))
					(ty-apply
						(ty (name "IsFruit"))
						(ty (name "Bool"))))))
		(s-decl
			(p-ident (raw "apple"))
			(e-tag (raw "Apple")))
		(e-ident (raw "apple"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "apple"))
		(e-tag (name "Apple")))
	(e-lookup-local
		(p-assign (ident "apple"))))
~~~
# TYPES
~~~clojure
(expr (type "[Apple][IsFruit(Bool)]"))
~~~
