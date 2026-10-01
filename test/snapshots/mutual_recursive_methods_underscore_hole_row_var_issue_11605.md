# META
~~~ini
description=Mutually recursive methods annotated with a _ hole and a shared named row variable type check (issue 11605)
type=file
~~~
# SOURCE
~~~roc
say : Str -> Str
say = |text| text

Client(effects) := { effects : effects }.{
    command : Client(_), Str, U64 -> Try(Str, [Refused(Str), ..others])
    command = |client, sql, attempts| {
        _ = say(sql)
        if attempts == 0 Ok("done") else retry(client, sql, attempts - 1)
    }

    retry : Client(_), Str, U64 -> Try(Str, [Refused(Str), ..others])
    retry = |client, sql, attempts| command(client, sql, attempts)
}

run : Str -> Try(Str, [Refused(Str)])
run = |sql| {
    client = Client.{ effects: {} }
    done = Client.command(client, sql, 1)?
    Ok(done)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
KwIf,LowerIdent,OpEquals,Int,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBinaryMinus,Int,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,Int,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "say")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "say"))
			(e-lambda
				(args
					(p-ident (raw "text")))
				(e-ident (raw "text"))))
		(s-type-decl
			(header (name "Client")
				(args
					(ty-var (raw "effects"))))
			(ty-record
				(anno-record-field (name "effects")
					(ty-var (raw "effects"))))
			(associated
				(s-type-anno (name "command")
					(ty-fn
						(ty-apply
							(ty (name "Client"))
							(_))
						(ty (name "Str"))
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
					(p-ident (raw "command"))
					(e-lambda
						(args
							(p-ident (raw "client"))
							(p-ident (raw "sql"))
							(p-ident (raw "attempts")))
						(e-block
							(statements
								(s-decl
									(p-underscore)
									(e-apply
										(e-ident (raw "say"))
										(e-ident (raw "sql"))))
								(e-if-then-else
									(e-binop (op "==")
										(e-ident (raw "attempts"))
										(e-int (raw "0")))
									(e-apply
										(e-tag (raw "Ok"))
										(e-string
											(e-string-part (raw "done"))))
									(e-apply
										(e-ident (raw "retry"))
										(e-ident (raw "client"))
										(e-ident (raw "sql"))
										(e-binop (op "-")
											(e-ident (raw "attempts"))
											(e-int (raw "1")))))))))
				(s-type-anno (name "retry")
					(ty-fn
						(ty-apply
							(ty (name "Client"))
							(_))
						(ty (name "Str"))
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
					(p-ident (raw "retry"))
					(e-lambda
						(args
							(p-ident (raw "client"))
							(p-ident (raw "sql"))
							(p-ident (raw "attempts")))
						(e-apply
							(e-ident (raw "command"))
							(e-ident (raw "client"))
							(e-ident (raw "sql"))
							(e-ident (raw "attempts")))))))
		(s-type-anno (name "run")
			(ty-fn
				(ty (name "Str"))
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
					(p-ident (raw "sql")))
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
									(e-ident (raw "Client.command"))
									(e-ident (raw "client"))
									(e-ident (raw "sql"))
									(e-int (raw "1")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "done")))))))))
~~~
# FORMATTED
~~~roc
say : Str -> Str
say = |text| text

Client(effects) := { effects : effects }.{
	command : Client(_), Str, U64 -> Try(Str, [Refused(Str), ..others])
	command = |client, sql, attempts| {
		_ = say(sql)
		if attempts == 0 Ok("done") else retry(client, sql, attempts - 1)
	}

	retry : Client(_), Str, U64 -> Try(Str, [Refused(Str), ..others])
	retry = |client, sql, attempts| command(client, sql, attempts)
}

run : Str -> Try(Str, [Refused(Str)])
run = |sql| {
	client = Client.{ effects: {} }
	done = Client.command(client, sql, 1)?
	Ok(done)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "say"))
		(e-lambda
			(args
				(p-assign (ident "text")))
			(e-lookup-local
				(p-assign (ident "text"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "mutual_recursive_methods_underscore_hole_row_var_issue_11605.Client.command"))
		(e-lambda
			(args
				(p-assign (ident "client"))
				(p-assign (ident "sql"))
				(p-assign (ident "attempts")))
			(e-block
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 404)
						(e-lookup-local
							(p-assign (ident "say")))
						(e-lookup-local
							(p-assign (ident "sql")))))
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "attempts"))))
								(rhs
									(e-num (value "0"))))
							(e-tag (name "Ok")
								(args
									(e-string
										(e-literal (string "done")))))))
					(if-else
						(e-call (constraint-fn-var 456)
							(e-lookup-local
								(p-assign (ident "mutual_recursive_methods_underscore_hole_row_var_issue_11605.Client.retry")))
							(e-lookup-local
								(p-assign (ident "client")))
							(e-lookup-local
								(p-assign (ident "sql")))
							(e-dispatch-call (method "minus") (constraint-fn-var 450)
								(receiver
									(e-lookup-local
										(p-assign (ident "attempts"))))
								(args
									(e-num (value "1")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Client") (local)
					(ty-underscore))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin)))
						(ty-rigid-var (name "others")))))))
	(d-let
		(p-assign (ident "mutual_recursive_methods_underscore_hole_row_var_issue_11605.Client.retry"))
		(e-lambda
			(args
				(p-assign (ident "client"))
				(p-assign (ident "sql"))
				(p-assign (ident "attempts")))
			(e-call (constraint-fn-var 478)
				(e-lookup-local
					(p-assign (ident "mutual_recursive_methods_underscore_hole_row_var_issue_11605.Client.command")))
				(e-lookup-local
					(p-assign (ident "client")))
				(e-lookup-local
					(p-assign (ident "sql")))
				(e-lookup-local
					(p-assign (ident "attempts")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Client") (local)
					(ty-underscore))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin)))
						(ty-rigid-var (name "others")))))))
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "sql")))
			(e-block
				(s-let
					(p-assign (ident "client"))
					(e-nominal (nominal "Client")
						(e-record
							(fields
								(field (name "effects")
									(e-empty_record))))))
				(s-let
					(p-assign (ident "done"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 520)
									(e-lookup-local
										(p-assign (ident "mutual_recursive_methods_underscore_hole_row_var_issue_11605.Client.command")))
									(e-lookup-local
										(p-assign (ident "client")))
									(e-lookup-local
										(p-assign (ident "sql")))
									(e-num (value "1"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-return
											(e-nominal-external
												(builtin)
												(e-tag (name "Err")
													(args
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "done")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
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
		(patt (type "Str -> Str"))
		(patt (type "Client(_a), Str, U64 -> Try(Str, [Refused(Str), ..others])"))
		(patt (type "Client(_a), Str, U64 -> Try(Str, [Refused(Str), ..others])"))
		(patt (type "Str -> Try(Str, [Refused(Str)])")))
	(type_decls
		(nominal (type "Client(effects)")
			(ty-header (name "Client")
				(ty-args
					(ty-rigid-var (name "effects"))))))
	(expressions
		(expr (type "Str -> Str"))
		(expr (type "Client(_a), Str, U64 -> Try(Str, [Refused(Str), ..others])"))
		(expr (type "Client(_a), Str, U64 -> Try(Str, [Refused(Str), ..others])"))
		(expr (type "Str -> Try(Str, [Refused(Str)])"))))
~~~
