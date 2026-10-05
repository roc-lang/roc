# META
~~~ini
description=Derived == on an open tag row requires is_eq on its tail, so instantiating the tail with a function payload is rejected like a direct comparison of that union
type=snippet
~~~
# SOURCE
~~~roc
same = |a, b| if a == Nope { False } else { a == b }

x = same(Fn(|z| z), Fn(|z| z))
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - derived_eq_open_row_rejects_function_tail.md:1:18:1:27
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Does Not Support Equality")
		(region (start 1 18) (end 1 27))
		(headline
			(reflow "This expression is doing an equality check on a type that doesn't support equality."))
		(document
			(source-region (file "derived_eq_open_row_rejects_function_tail.md") (start 1 18) (end 1 27) (annotation error) (line-text "same = |a, b| if a == Nope { False } else { a == b }"))
			(line-break)
			(reflow "The type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Fn(c -> c)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "This tag union does not support equality because these tags have payload types that don't support ")
			(annotated emphasis "is_eq")
			(reflow ":")
			(line-break)
			(line-break)
			(text "    ")
			(annotated emphasis "Fn")
			(text " (")
			(annotated type "c -> c")
			(text ")")
			(line-break)
			(text "        ")
			(reflow "Function equality is not supported.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Tag unions only have an ")
			(annotated emphasis "is_eq")
			(reflow " method if all of their payload types have ")
			(annotated emphasis "is_eq")
			(reflow " methods.")
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,UpperIdent,OpenCurly,UpperIdent,CloseCurly,KwElse,OpenCurly,LowerIdent,OpEquals,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "same"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "a"))
						(e-tag (raw "Nope")))
					(e-block
						(statements
							(e-tag (raw "False"))))
					(e-block
						(statements
							(e-binop (op "==")
								(e-ident (raw "a"))
								(e-ident (raw "b"))))))))
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-ident (raw "same"))
				(e-apply
					(e-tag (raw "Fn"))
					(e-lambda
						(args
							(p-ident (raw "z")))
						(e-ident (raw "z"))))
				(e-apply
					(e-tag (raw "Fn"))
					(e-lambda
						(args
							(p-ident (raw "z")))
						(e-ident (raw "z"))))))))
~~~
# FORMATTED
~~~roc
same = |a, b| if a == Nope {
	False
} else {
	a == b
}

x = same(Fn(|z| z), Fn(|z| z))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "same"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-if
				(if-branches
					(if-branch
						(e-structural-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "a"))))
							(rhs
								(e-tag (name "Nope"))))
						(e-block
							(e-tag (name "False")))))
				(if-else
					(e-block
						(e-runtime-error (tag "erroneous_value_expr")))))))
	(d-let
		(p-assign (ident "x"))
		(e-call (constraint-fn-var 278)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-tag (name "Fn")
				(args
					(e-lambda
						(args
							(p-assign (ident "z")))
						(e-lookup-local
							(p-assign (ident "z"))))))
			(e-tag (name "Fn")
				(args
					(e-lambda
						(args
							(p-assign (ident "z")))
						(e-lookup-local
							(p-assign (ident "z")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Nope, ..c], [Nope, ..c] -> Bool where [c.is_eq : c, c -> Bool]"))
		(patt (type "Bool")))
	(expressions
		(expr (type "[Nope, ..c], [Nope, ..c] -> Bool where [c.is_eq : c, c -> Bool]"))
		(expr (type "Bool"))))
~~~
