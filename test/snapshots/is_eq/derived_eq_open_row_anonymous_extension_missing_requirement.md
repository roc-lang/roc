# META
~~~ini
description=Comparing a tag union whose annotation has an anonymous `..` explains that the `..` may stand for tags without is_eq and how to name it and require is_eq
type=snippet
~~~
# SOURCE
~~~roc
is_nope : [Nope, ..] -> Bool
is_nope = |v| v == Nope
~~~
# EXPECTED
MISSING METHOD - derived_eq_open_row_anonymous_extension_missing_requirement.md:2:15:2:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 2 15) (end 2 24))
		(headline
			(reflow "This equality check can't compare these values, because their type is")
			(reflow " ")
			(reflow "a tag union with")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "in it."))
		(document
			(source-region (file "derived_eq_open_row_anonymous_extension_missing_requirement.md") (start 2 15) (end 2 24) (annotation error) (line-text "is_nope = |v| v == Nope"))
			(line-break)
			(reflow "The values being compared have this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Nope, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "means this")
			(reflow " ")
			(reflow "tag union may have arbitrary other tags in addition to the ones written here, and those other tags don't necessarily have")
			(reflow " ")
			(reflow "an")
			(reflow " ")
			(annotated code "is_eq")
			(reflow " ")
			(reflow "method.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "To compare them anyway,")
			(reflow " ")
			(reflow "give the")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "a name in the type annotation (for example")
			(reflow " ")
			(annotated code "..others")
			(reflow ") and require")
			(reflow " ")
			(annotated code "is_eq")
			(reflow " ")
			(reflow "on it by adding this to the annotation:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "where [others.is_eq : others, others -> Bool]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,DoubleDot,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpEquals,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "is_nope")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "Nope")))
					..)
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "is_nope"))
			(e-lambda
				(args
					(p-ident (raw "v")))
				(e-binop (op "==")
					(e-ident (raw "v"))
					(e-tag (raw "Nope")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "is_nope"))
		(e-lambda
			(args
				(p-assign (ident "v")))
			(e-runtime-error (tag "erroneous_value_expr")
				(e-lookup-local
					(p-assign (ident "v")))
				(e-tag (name "Nope"))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Nope"))
					(ty-rigid-var (name "#others")))
				(ty-lookup (name "Bool") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Nope, ..] -> Bool")))
	(expressions
		(expr (type "[Nope, ..] -> Bool"))))
~~~
