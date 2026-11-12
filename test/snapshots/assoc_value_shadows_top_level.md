# META
~~~ini
description=An associated value named like a top-level function shadows it, and its RHS reference resolves to the top-level function (the item being defined never satisfies its own lookup)
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
DUPLICATE DEFINITION - assoc_value_shadows_top_level.md:2:5:2:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 2 5) (end 2 11))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "helper")
			(reflow " is being redeclared here."))
		(document
			(source-region (file "assoc_value_shadows_top_level.md") (start 2 5) (end 2 11) (annotation error) (line-text "    helper = helper"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "helper")
			(reflow " was already defined in ")
			(source-location
				(file "assoc_value_shadows_top_level.md")
				(line 5)
				(column 1))
			(reflow ".")
			(line-break)
			(source-region (file "assoc_value_shadows_top_level.md") (start 5 1) (end 5 7) (annotation dim) (line-text "helper = |x| x")))))
~~~
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
	(type-mod)
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
