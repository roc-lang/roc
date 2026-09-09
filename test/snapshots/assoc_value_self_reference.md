# META
~~~ini
description=An associated value whose RHS references the value being defined reports INVALID ASSIGNMENT TO ITSELF instead of manufacturing a self-cycle (issue 9912)
type=file:SelfRef.roc
~~~
# SOURCE
~~~roc
SelfRef := [].{
    with_uri = with_uri
}
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - assoc_value_self_reference.md:2:16:2:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 2 16) (end 2 24))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "with_uri")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "assoc_value_self_reference.md") (start 2 16) (end 2 24) (annotation error) (line-text "    with_uri = with_uri")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "SelfRef")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-decl
					(p-ident (raw "with_uri"))
					(e-ident (raw "with_uri")))))))
~~~
# FORMATTED
~~~roc
SelfRef := [].{
	with_uri = with_uri
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "SelfRef.with_uri"))
		(e-runtime-error (tag "self_referential_definition")))
	(s-nominal-decl
		(ty-header (name "SelfRef"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "SelfRef")
			(ty-header (name "SelfRef"))))
	(expressions
		(expr (type "Error"))))
~~~
