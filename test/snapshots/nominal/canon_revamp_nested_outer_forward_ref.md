# META
~~~ini
description=Nested associated block uses a parent-block item that is defined AFTER the nested type in source order
type=file:Outer.roc
~~~
# SOURCE
~~~roc
Outer := [Whatever].{
    Inner := [Other].{
        usesParent : Outer
        usesParent = parentItem
    }

    parentItem : Outer
    parentItem = Whatever
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Other"))))
					(associated
						(s-type-anno (name "usesParent")
							(ty (name "Outer")))
						(s-decl
							(p-ident (raw "usesParent"))
							(e-ident (raw "parentItem")))))
				(s-type-anno (name "parentItem")
					(ty (name "Outer")))
				(s-decl
					(p-ident (raw "parentItem"))
					(e-tag (raw "Whatever")))))))
~~~
# FORMATTED
~~~roc
Outer := [Whatever].{
	Inner := [Other].{
		usesParent : Outer
		usesParent = parentItem
	}

	parentItem : Outer
	parentItem = Whatever
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Outer.Inner.usesParent"))
		(e-lookup-local
			(p-assign (ident "Outer.parentItem")))
		(annotation
			(ty-lookup (name "Outer") (local))))
	(d-let
		(p-assign (ident "Outer.parentItem"))
		(e-tag (name "Whatever"))
		(annotation
			(ty-lookup (name "Outer") (local))))
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Outer.Inner"))
		(ty-tag-union
			(ty-tag-name (name "Other")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Outer"))
		(patt (type "Outer")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Inner")
			(ty-header (name "Outer.Inner"))))
	(expressions
		(expr (type "Outer"))
		(expr (type "Outer"))))
~~~
