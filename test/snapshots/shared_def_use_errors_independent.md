# META
~~~ini
description=A type error at one use of a top-level definition does not poison the definition, so a second bad use is still reported
type=snippet
~~~
# SOURCE
~~~roc
parse : Str -> Try(U64, [BadInput])
parse = |s| if s == "" Err(BadInput) else Ok(1)

first_use : Str -> Str
first_use = |s| {
	n : Str
	n = parse(s)
	n
}

second_use : Str -> Str
second_use = |s| {
	m : Str
	m = parse(s)
	m
}
~~~
# EXPECTED
TYPE MISMATCH - shared_def_use_errors_independent.md:7:6:7:14
TYPE MISMATCH - shared_def_use_errors_independent.md:14:6:14:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 6) (end 7 14))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "shared_def_use_errors_independent.md") (start 7 6) (end 7 14) (annotation error) (line-text "\tn = parse(s)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(U64, [BadInput])")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 14 6) (end 14 14))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "shared_def_use_errors_independent.md") (start 14 6) (end 14 14) (annotation error) (line-text "\tm = parse(s)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(U64, [BadInput])")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "parse")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U64"))
					(ty-tag-union
						(tags
							(ty (name "BadInput")))))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "s"))
						(e-string
							(e-string-part (raw ""))))
					(e-apply
						(e-tag (raw "Err"))
						(e-tag (raw "BadInput")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-int (raw "1"))))))
		(s-type-anno (name "first_use")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "first_use"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-type-anno (name "n")
							(ty (name "Str")))
						(s-decl
							(p-ident (raw "n"))
							(e-apply
								(e-ident (raw "parse"))
								(e-ident (raw "s"))))
						(e-ident (raw "n"))))))
		(s-type-anno (name "second_use")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "second_use"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-type-anno (name "m")
							(ty (name "Str")))
						(s-decl
							(p-ident (raw "m"))
							(e-apply
								(e-ident (raw "parse"))
								(e-ident (raw "s"))))
						(e-ident (raw "m"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-if
				(if-branches
					(if-branch
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "s"))))
							(rhs
								(e-string
									(e-literal (string "")))))
						(e-tag (name "Err")
							(args
								(e-tag (name "BadInput"))))))
				(if-else
					(e-tag (name "Ok")
						(args
							(e-num (value "1")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadInput")))))))
	(d-let
		(p-assign (ident "first_use"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "n"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-lookup-local
					(p-assign (ident "n")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "second_use"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "m"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-lookup-local
					(p-assign (ident "m")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(U64, [BadInput])"))
		(patt (type "Str -> Str"))
		(patt (type "Str -> Str")))
	(expressions
		(expr (type "Str -> Try(U64, [BadInput])"))
		(expr (type "Str -> Str"))
		(expr (type "Str -> Str"))))
~~~
