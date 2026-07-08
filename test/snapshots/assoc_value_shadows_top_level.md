# META
~~~ini
description=An associated value named like a top-level function resolves its RHS reference to the top-level function (the item being defined never satisfies its own lookup)
type=file:Shadow.roc
~~~
# SOURCE
~~~roc
Shadow := [].{
    helper = helper
}

helper = |x| x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Shadow")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-decl
					(p-ident (raw "helper"))
					(e-ident (raw "helper")))))
		(s-decl
			(p-ident (raw "helper"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))))
~~~
# FORMATTED
~~~roc
Shadow := [].{
	helper = helper
}

helper = |x| x
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Shadow.helper"))
		(e-lookup-local
			(p-assign (ident "helper"))))
	(d-let
		(p-assign (ident "helper"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(s-nominal-decl
		(ty-header (name "Shadow"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "a -> a")))
	(type_decls
		(nominal (type "Shadow")
			(ty-header (name "Shadow"))))
	(expressions
		(expr (type "a -> a"))
		(expr (type "a -> a"))))
~~~
