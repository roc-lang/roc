# META
~~~ini
description=Method forward-references an anno-only sibling within the same associated block
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    callMe : Foo
    callMe = absent

    absent : Foo
}
~~~
# EXPECTED
DECLARATION HAS NO VALUE - canon_revamp_forward_ref_to_anno_only.md:5:5:5:17
REFERENCE HAS NO VALUE - canon_revamp_forward_ref_to_anno_only.md:3:14:3:20
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 5 5) (end 5 17))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "canon_revamp_forward_ref_to_anno_only.md") (start 5 5) (end 5 17) (annotation error) (line-text "    absent : Foo"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Reference Has No Value")
		(region (start 3 14) (end 3 20))
		(headline
			(reflow "This refers to a declaration that has a type annotation but no implementation, so there is no value here to use."))
		(document
			(source-region (file "canon_revamp_forward_ref_to_anno_only.md") (start 3 14) (end 3 20) (annotation error) (line-text "    callMe = absent"))
			(line-break)
			(line-break)
			(reflow "Give that declaration a value body, or stop referring to it here."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-type-anno (name "callMe")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "callMe"))
					(e-ident (raw "absent")))
				(s-type-anno (name "absent")
					(ty (name "Foo")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	callMe : Foo
	callMe = absent

	absent : Foo
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.callMe"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "Foo") (local))))
	(d-let
		(p-assign (ident "Foo.absent"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "Foo") (local))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo"))
		(patt (type "Foo")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Foo"))
		(expr (type "Foo"))))
~~~
