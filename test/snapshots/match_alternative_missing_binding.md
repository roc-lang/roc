# META
~~~ini
description=A name bound in one `|` alternative but not another is reported as a missing binding, not a type mismatch
type=snippet
~~~
# SOURCE
~~~roc
bound_first : [A(U8), B] -> U8
bound_first = |t| match t {
    A(n) | B => n
}

bound_second : [A(U8), B] -> U8
bound_second = |t| match t {
    B | A(n) => n
}
~~~
# EXPECTED
NAME NOT IN SCOPE - match_alternative_missing_binding.md:8:17:8:18
NAME NOT BOUND IN EVERY ALTERNATIVE - match_alternative_missing_binding.md:3:12:3:13
NAME NOT BOUND IN EVERY ALTERNATIVE - match_alternative_missing_binding.md:8:5:8:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 8 17) (end 8 18))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "n")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "match_alternative_missing_binding.md") (start 8 17) (end 8 18) (annotation error) (line-text "    B | A(n) => n"))))
	(report
		(severity runtime_error)
		(title "Name Not Bound In Every Alternative")
		(region (start 3 12) (end 3 13))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "gives a value the name")
			(reflow " ")
			(annotated code "n")
			(reflow ", but the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern does not."))
		(document
			(source-region (file "match_alternative_missing_binding.md") (start 3 12) (end 3 13) (annotation error) (line-text "    A(n) | B => n"))
			(line-break)
			(reflow "Every pattern separated by")
			(reflow " ")
			(annotated code "|")
			(reflow " ")
			(reflow "in a branch must give values the same names, so the branch can use those names no matter which pattern matched.")))
	(report
		(severity runtime_error)
		(title "Name Not Bound In Every Alternative")
		(region (start 8 5) (end 8 6))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "gives a value the name")
			(reflow " ")
			(annotated code "n")
			(reflow ", but the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern does not."))
		(document
			(source-region (file "match_alternative_missing_binding.md") (start 8 5) (end 8 6) (annotation error) (line-text "    B | A(n) => n"))
			(line-break)
			(reflow "Every pattern separated by")
			(reflow " ")
			(annotated code "|")
			(reflow " ")
			(reflow "in a branch must give values the same names, so the branch can use those names no matter which pattern matched."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "bound_first")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "A"))
							(ty (name "U8")))
						(ty (name "B"))))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "bound_first"))
			(e-lambda
				(args
					(p-ident (raw "t")))
				(e-match
					(e-ident (raw "t"))
					(branches
						(branch
							(p-alternatives
								(p-tag (raw "A")
									(p-ident (raw "n")))
								(p-tag (raw "B")))
							(e-ident (raw "n")))))))
		(s-type-anno (name "bound_second")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "A"))
							(ty (name "U8")))
						(ty (name "B"))))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "bound_second"))
			(e-lambda
				(args
					(p-ident (raw "t")))
				(e-match
					(e-ident (raw "t"))
					(branches
						(branch
							(p-alternatives
								(p-tag (raw "B"))
								(p-tag (raw "A")
									(p-ident (raw "n"))))
							(e-ident (raw "n")))))))))
~~~
# FORMATTED
~~~roc
bound_first : [A(U8), B] -> U8
bound_first = |t| match t {
	A(n) | B => n
}

bound_second : [A(U8), B] -> U8
bound_second = |t| match t {
	B | A(n) => n
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "bound_first"))
		(e-lambda
			(args
				(p-assign (ident "t")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A")
						(ty-lookup (name "U8") (builtin)))
					(ty-tag-name (name "B")))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "bound_second"))
		(e-lambda
			(args
				(p-assign (ident "t")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A")
						(ty-lookup (name "U8") (builtin)))
					(ty-tag-name (name "B")))
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[A(U8), B] -> U8"))
		(patt (type "[A(U8), B] -> U8")))
	(expressions
		(expr (type "[A(U8), B] -> U8"))
		(expr (type "[A(U8), B] -> U8"))))
~~~
