# META
~~~ini
description=An annotation comparing an open tag row must declare the tail's is_eq requirement
type=snippet
~~~
# SOURCE
~~~roc
is_nope : [Nope, ..a] -> Bool
is_nope = |v| v == Nope
~~~
# EXPECTED
MISSING METHOD - derived_eq_open_row_annotation_missing_requirement.md:2:15:2:24
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
			(source-region (file "derived_eq_open_row_annotation_missing_requirement.md") (start 2 15) (end 2 24) (annotation error) (line-text "is_nope = |v| v == Nope"))
			(line-break)
			(reflow "The values being compared have this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Nope, ..a]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "..a")
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
			(reflow "require")
			(reflow " ")
			(annotated code "is_eq")
			(reflow " ")
			(reflow "on")
			(reflow " ")
			(annotated code "a")
			(reflow " ")
			(reflow "by adding this to the type annotation:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "where [a.is_eq : a, a -> Bool]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpArrow,UpperIdent,
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
					(ty-var (raw "a")))
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
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Nope"))
					(ty-rigid-var (name "a")))
				(ty-lookup (name "Bool") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Nope, ..a] -> Bool")))
	(expressions
		(expr (type "[Nope, ..a] -> Bool"))))
~~~
