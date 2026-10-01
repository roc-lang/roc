# META
~~~ini
description=Derived == on a record with a generic field must require is_eq on that field like a direct comparison does (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
eq : a, a -> Bool
eq = |x, y| { n: x } == { n: y }

x = eq(|z| z, |z| z)
~~~
# EXPECTED
MISSING METHOD - derived_eq_generic_component_issue_11575.md:2:13:2:33
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 2 13) (end 2 33))
		(headline
			(reflow "The value before this")
			(reflow " ")
			(annotated operator "==")
			(reflow " ")
			(reflow "operator has a type that doesn't have a")
			(reflow " ")
			(annotated code "is_eq")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "derived_eq_generic_component_issue_11575.md") (start 2 13) (end 2 33) (annotation error) (line-text "eq = |x, y| { n: x } == { n: y }"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "is_eq")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The")
			(reflow " ")
			(annotated operator "==")
			(reflow " ")
			(reflow "operator requires the type to have a")
			(reflow " ")
			(annotated code "is_eq")
			(reflow " ")
			(reflow "method. Did you forget to specify it in the type annotation?"))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,OpEquals,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,Comma,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "eq")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "eq"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-binop (op "==")
					(e-record
						(field (field "n")
							(e-ident (raw "x"))))
					(e-record
						(field (field "n")
							(e-ident (raw "y")))))))
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-ident (raw "eq"))
				(e-lambda
					(args
						(p-ident (raw "z")))
					(e-ident (raw "z")))
				(e-lambda
					(args
						(p-ident (raw "z")))
					(e-ident (raw "z")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "eq"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "x"))
		(e-call (constraint-fn-var 250)
			(e-lookup-local
				(p-assign (ident "eq")))
			(e-lambda
				(args
					(p-assign (ident "z")))
				(e-lookup-local
					(p-assign (ident "z"))))
			(e-lambda
				(args
					(p-assign (ident "z")))
				(e-lookup-local
					(p-assign (ident "z")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, a -> Bool"))
		(patt (type "Bool")))
	(expressions
		(expr (type "a, a -> Bool"))
		(expr (type "Bool"))))
~~~
