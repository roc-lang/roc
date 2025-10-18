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
UNDECLARED TYPE - ann_closed_union.md:2:26:2:30
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**ann_closed_union.md:2:26:2:30:**
```roc
	apple : [Apple, IsFruit(Bool)]
```
	                        ^^^^


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
(expr (type "[Apple][IsFruit(Error)]"))
~~~
