# META
~~~ini
description=Derived == on an open tag row checks the payloads its extension supplies, so a Box payload (which has no is_eq) is rejected even when the compared literal names another tag
type=snippet
~~~
# SOURCE
~~~roc
b1 : [Nope, Yep(Box(U64))]
b1 = Yep(Box.box(3))

x = b1 == Nope
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - derived_eq_open_row_extension_payload_without_is_eq.md:4:5:4:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Does Not Support Equality")
		(region (start 4 5) (end 4 15))
		(headline
			(reflow "This expression is doing an equality check on a type that doesn't support equality."))
		(document
			(source-region (file "derived_eq_open_row_extension_payload_without_is_eq.md") (start 4 5) (end 4 15) (annotation error) (line-text "x = b1 == Nope"))
			(line-break)
			(reflow "The type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Nope, Yep(Box(U64))]")
			(annotation-end)
			(line-break)
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,OpEquals,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "b1")
			(ty-tag-union
				(tags
					(ty (name "Nope"))
					(ty-apply
						(ty (name "Yep"))
						(ty-apply
							(ty (name "Box"))
							(ty (name "U64")))))))
		(s-decl
			(p-ident (raw "b1"))
			(e-apply
				(e-tag (raw "Yep"))
				(e-apply
					(e-ident (raw "Box.box"))
					(e-int (raw "3")))))
		(s-decl
			(p-ident (raw "x"))
			(e-binop (op "==")
				(e-ident (raw "b1"))
				(e-tag (raw "Nope"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b1"))
		(e-tag (name "Yep")
			(args
				(e-call (constraint-fn-var 255)
					(e-lookup-external
						(builtin))
					(e-num (value "3")))))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Nope"))
				(ty-tag-name (name "Yep")
					(ty-apply (name "Box") (builtin)
						(ty-lookup (name "U64") (builtin)))))))
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Nope, Yep(Box(U64))]"))
		(patt (type "Bool")))
	(expressions
		(expr (type "[Nope, Yep(Box(U64))]"))
		(expr (type "Bool"))))
~~~
