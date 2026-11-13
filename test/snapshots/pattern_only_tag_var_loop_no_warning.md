# META
~~~ini
description=A tag a var is assigned after a loop body's match was checked is produced, so the match pattern is not reported
type=file
~~~
# SOURCE
~~~roc
main! = |args| {
	var $state = Idle
	var $busy_count = 0

	for arg in args {
		match $state {
			Busy => {
				$busy_count = $busy_count + 1
			}
			Idle => {}
		}

		if arg == "start" {
			$state = Busy
		}
	}

	echo!("busy for ${$busy_count.to_str()} args")
	Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,UpperIdent,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
UpperIdent,OpFatArrow,OpenCurly,CloseCurly,
CloseCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,OpenCurly,
LowerIdent,OpAssign,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "args")))
				(e-block
					(statements
						(s-var (name "$state")
							(e-tag (raw "Idle")))
						(s-var (name "$busy_count")
							(e-int (raw "0")))
						(s-for
							(p-ident (raw "arg"))
							(e-ident (raw "args"))
							(e-block
								(statements
									(e-match
										(e-ident (raw "$state"))
										(branches
											(branch
												(p-tag (raw "Busy"))
												(e-block
													(statements
														(s-decl
															(p-ident (raw "$busy_count"))
															(e-binop (op "+")
																(e-ident (raw "$busy_count"))
																(e-int (raw "1")))))))
											(branch
												(p-tag (raw "Idle"))
												(e-record))))
									(e-if-without-else
										(e-binop (op "==")
											(e-ident (raw "arg"))
											(e-string
												(e-string-part (raw "start"))))
										(e-block
											(statements
												(s-decl
													(p-ident (raw "$state"))
													(e-tag (raw "Busy")))))))))
						(e-apply
							(e-ident (raw "echo!"))
							(e-string
								(e-string-part (raw "busy for "))
								(e-method-call (method ".to_str")
									(receiver
										(e-ident (raw "$busy_count")))
									(args))
								(e-string-part (raw " args"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "args")))
			(e-block
				(s-var
					(p-var-assign (ident "$state"))
					(e-tag (name "Idle")))
				(s-var
					(p-var-assign (ident "$busy_count"))
					(e-num (value "0")))
				(s-for
					(p-assign (ident "arg"))
					(e-lookup-local
						(p-assign (ident "args")))
					(e-block
						(s-expr
							(e-match
								(match
									(cond
										(e-lookup-local
											(p-var-assign (ident "$state"))))
									(branches
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-block
													(s-reassign
														(p-var-assign (ident "$busy_count"))
														(e-dispatch-call (method "plus") (constraint-fn-var 341)
															(receiver
																(e-lookup-local
																	(p-var-assign (ident "$busy_count"))))
															(args
																(e-num (value "1")))))
													(e-empty_record))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-empty_record)))))))
						(s-expr
							(e-if
								(if-branches
									(if-branch
										(e-method-eq (negated "false")
											(lhs
												(e-lookup-local
													(p-assign (ident "arg"))))
											(rhs
												(e-string
													(e-literal (string "start")))))
										(e-block
											(s-reassign
												(p-var-assign (ident "$state"))
												(e-tag (name "Busy")))
											(e-empty_record))))
								(if-else
									(e-empty_record))))
						(e-empty_record)))
				(s-expr
					(e-call (constraint-fn-var 407)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-block
							(s-let
								(p-assign (ident "#interp_0"))
								(e-dispatch-call (method "to_str") (constraint-fn-var 367)
									(receiver
										(e-lookup-local
											(p-var-assign (ident "$busy_count"))))
									(args)))
							(e-interpolation (constraint-fn-var 384) (dispatcher-var 58)
								(first
									(e-literal (string "busy for ")))
								(parts
									(e-lookup-local
										(p-assign (ident "#interp_0")))
									(e-literal (string " args")))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "a => [Ok({})] where [a.iter : a -> Iter(item), item.from_quote : Str -> Try(item, [BadQuotedBytes(Str)]), item.is_eq : item, item -> Bool]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "a => [Ok({})] where [a.iter : a -> Iter(item), item.from_quote : Str -> Try(item, [BadQuotedBytes(Str)]), item.is_eq : item, item -> Bool]"))))
~~~
