# META
~~~ini
description=An explicit fractional pattern suffix constrains the pattern independently of its match context
type=snippet
~~~
# SOURCE
~~~roc
classify : F64 -> I64
classify = |n| match n {
	1.5.F32 => 1
	_ => 0
}
~~~
# EXPECTED
TYPE MISMATCH - issue_10134_typed_frac_pattern_suffix_mismatch.md:2:16:2:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 16) (end 5 2))
		(headline
			(reflow "The first pattern in this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "is incompatible."))
		(document
			(source-underlines
				(display (file "issue_10134_typed_frac_pattern_suffix_mismatch.md") (start 2 16) (end 5 2) (annotation dim) (line-text "classify = |n| match n {\n\t1.5.F32 => 1\n\t_ => 0\n}"))
				(underline (start 3 2) (end 3 9) (annotation error)))
			(line-break)
			(reflow "The first pattern is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "F32")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the expression between the")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "parenthesis has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "F64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Float,NoSpaceDotUpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "classify")
			(ty-fn
				(ty (name "F64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "classify"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-match
					(e-ident (raw "n"))
					(branches
						(branch
							(p-typed-frac (raw "1.5") (type "F32"))
							(e-int (raw "1")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "classify"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "F64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "F64 -> I64")))
	(expressions
		(expr (type "F64 -> I64"))))
~~~
