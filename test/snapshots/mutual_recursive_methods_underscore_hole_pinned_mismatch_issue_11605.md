# META
~~~ini
description=A _ hole pinned by one mutually recursive member's body constrains callers of the other member (issue 11605)
type=file
~~~
# SOURCE
~~~roc
Client(effects) := { effects : effects }.{
    first : Client(_), List(_), U64 -> Try(Str, [Refused(Str), ..others])
    first = |client, items, n| {
        pinned : List(Str)
        pinned = items
        if n == 0 Ok(Str.join_with(pinned, "")) else second(client, items, n - 1)
    }

    second : Client(_), List(_), U64 -> Try(Str, [Refused(Str), ..others])
    second = |client, items, n| first(client, items, n)
}

run : U64 -> Try(Str, [Refused(Str)])
run = |num| {
    client = Client.{ effects: {} }
    done = Client.second(client, [num], 1)?
    Ok(done)
}
~~~
# EXPECTED
TYPE MISMATCH - mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.md:16:12:16:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 16 12) (end 16 43))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.md") (start 16 12) (end 16 43) (annotation dim) (line-text "    done = Client.second(client, [num], 1)?"))
				(underline (start 16 34) (end 16 39) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.Client.second")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
KwIf,LowerIdent,OpEquals,Int,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseRound,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBinaryMinus,Int,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,LowerIdent,CloseSquare,Comma,Int,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Client")
				(args
					(ty-var (raw "effects"))))
			(ty-record
				(anno-record-field (name "effects")
					(ty-var (raw "effects"))))
			(associated
				(s-type-anno (name "first")
					(ty-fn
						(ty-apply
							(ty (name "Client"))
							(_))
						(ty-apply
							(ty (name "List"))
							(_))
						(ty (name "U64"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Refused"))
										(ty (name "Str"))))
								(ty-var (raw "others"))))))
				(s-decl
					(p-ident (raw "first"))
					(e-lambda
						(args
							(p-ident (raw "client"))
							(p-ident (raw "items"))
							(p-ident (raw "n")))
						(e-block
							(statements
								(s-type-anno (name "pinned")
									(ty-apply
										(ty (name "List"))
										(ty (name "Str"))))
								(s-decl
									(p-ident (raw "pinned"))
									(e-ident (raw "items")))
								(e-if-then-else
									(e-binop (op "==")
										(e-ident (raw "n"))
										(e-int (raw "0")))
									(e-apply
										(e-tag (raw "Ok"))
										(e-apply
											(e-ident (raw "Str.join_with"))
											(e-ident (raw "pinned"))
											(e-string
												(e-string-part (raw "")))))
									(e-apply
										(e-ident (raw "second"))
										(e-ident (raw "client"))
										(e-ident (raw "items"))
										(e-binop (op "-")
											(e-ident (raw "n"))
											(e-int (raw "1")))))))))
				(s-type-anno (name "second")
					(ty-fn
						(ty-apply
							(ty (name "Client"))
							(_))
						(ty-apply
							(ty (name "List"))
							(_))
						(ty (name "U64"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Refused"))
										(ty (name "Str"))))
								(ty-var (raw "others"))))))
				(s-decl
					(p-ident (raw "second"))
					(e-lambda
						(args
							(p-ident (raw "client"))
							(p-ident (raw "items"))
							(p-ident (raw "n")))
						(e-apply
							(e-ident (raw "first"))
							(e-ident (raw "client"))
							(e-ident (raw "items"))
							(e-ident (raw "n")))))))
		(s-type-anno (name "run")
			(ty-fn
				(ty (name "U64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Refused"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "client"))
							(e-nominal-record
								(mapper (e-tag (raw "Client")))
								(backing (e-record
										(field (field "effects")
											(e-record))))))
						(s-decl
							(p-ident (raw "done"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "Client.second"))
									(e-ident (raw "client"))
									(e-list
										(e-ident (raw "num")))
									(e-int (raw "1")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "done")))))))))
~~~
# FORMATTED
~~~roc
Client(effects) := { effects : effects }.{
	first : Client(_), List(_), U64 -> Try(Str, [Refused(Str), ..others])
	first = |client, items, n| {
		pinned : List(Str)
		pinned = items
		if n == 0 Ok(Str.join_with(pinned, "")) else second(client, items, n - 1)
	}

	second : Client(_), List(_), U64 -> Try(Str, [Refused(Str), ..others])
	second = |client, items, n| first(client, items, n)
}

run : U64 -> Try(Str, [Refused(Str)])
run = |num| {
	client = Client.{ effects: {} }
	done = Client.second(client, [num], 1)?
	Ok(done)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.Client.first"))
		(e-lambda
			(args
				(p-assign (ident "client"))
				(p-assign (ident "items"))
				(p-assign (ident "n")))
			(e-block
				(s-let
					(p-assign (ident "pinned"))
					(e-lookup-local
						(p-assign (ident "items"))))
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "n"))))
								(rhs
									(e-num (value "0"))))
							(e-tag (name "Ok")
								(args
									(e-call (constraint-fn-var 433)
										(e-lookup-external
											(builtin))
										(e-lookup-local
											(p-assign (ident "pinned")))
										(e-string
											(e-literal (string ""))))))))
					(if-else
						(e-call (constraint-fn-var 455)
							(e-lookup-local
								(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.Client.second")))
							(e-lookup-local
								(p-assign (ident "client")))
							(e-lookup-local
								(p-assign (ident "items")))
							(e-dispatch-call (method "minus") (constraint-fn-var 449)
								(receiver
									(e-lookup-local
										(p-assign (ident "n"))))
								(args
									(e-num (value "1")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Client") (local)
					(ty-underscore))
				(ty-apply (name "List") (builtin)
					(ty-underscore))
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin)))
						(ty-rigid-var (name "others")))))))
	(d-let
		(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.Client.second"))
		(e-lambda
			(args
				(p-assign (ident "client"))
				(p-assign (ident "items"))
				(p-assign (ident "n")))
			(e-call (constraint-fn-var 478)
				(e-lookup-local
					(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_mismatch_issue_11605.Client.first")))
				(e-lookup-local
					(p-assign (ident "client")))
				(e-lookup-local
					(p-assign (ident "items")))
				(e-lookup-local
					(p-assign (ident "n")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Client") (local)
					(ty-underscore))
				(ty-apply (name "List") (builtin)
					(ty-underscore))
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin)))
						(ty-rigid-var (name "others")))))))
	(d-let
		(p-assign (ident "run"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin))))))))
	(s-nominal-decl
		(ty-header (name "Client")
			(ty-args
				(ty-rigid-var (name "effects"))))
		(ty-record
			(field (field "effects")
				(ty-rigid-var-lookup (ty-rigid-var (name "effects")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Client(_a), List(Str), U64 -> Try(Str, [Refused(Str), ..others])"))
		(patt (type "Client(_a), List(Str), U64 -> Try(Str, [Refused(Str), ..others])"))
		(patt (type "U64 -> Try(Str, [Refused(Str)])")))
	(type_decls
		(nominal (type "Client(effects)")
			(ty-header (name "Client")
				(ty-args
					(ty-rigid-var (name "effects"))))))
	(expressions
		(expr (type "Client(_a), List(Str), U64 -> Try(Str, [Refused(Str), ..others])"))
		(expr (type "Client(_a), List(Str), U64 -> Try(Str, [Refused(Str), ..others])"))
		(expr (type "U64 -> Try(Str, [Refused(Str)])"))))
~~~
