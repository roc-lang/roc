# META
~~~ini
description=The mirrored shape of issue 11469 is rejected: a bare try suffix in a higher-order function equates its callback's error row with the callee's
type=snippet
~~~
# SOURCE
~~~roc
# The mirrored shape of https://github.com/roc-lang/roc/issues/11469: the bare
# `?` is in the higher-order function and the tag-wrapped `?` is in the
# callback. `mirror_transaction` needs `operation`'s error row to INCLUDE
# `execute`'s, a bound between two otherwise free row variables that no
# equality-based scheme carries through generalization, so the checker equates
# them and a callback that re-tags `execute`'s errors needs a recursive row.
# This pins that rejection (design.md "Inferred Try Return-Row Composition").
mirror_transaction = |execute, operation| {
	_ = execute("BEGIN")?
	operation({})
}

mirror_save = |execute|
	mirror_transaction(
		execute,
		|{}| {
			_ = execute("INSERT") ? InsertFailed
			Ok({})
		},
	)

mirror_main = mirror_save(|_sql| Ok({}))
~~~
# EXPECTED
ANONYMOUS RECURSION - issue_11469_mirror_bare_try_in_higher_order_fn.md:13:1:20:3
ANONYMOUS RECURSION - issue_11469_mirror_bare_try_in_higher_order_fn.md:22:1:22:41
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Anonymous Recursion")
		(region (start 13 1) (end 20 3))
		(headline
			(reflow "I am inferring a recursive type that has no name somewhere in")
			(reflow " ")
			(annotated code "mirror_save")
			(reflow "."))
		(document
			(source-region (file "issue_11469_mirror_bare_try_in_higher_order_fn.md") (start 13 1) (end 20 3) (annotation error) (line-text "mirror_save = |execute|\n\tmirror_transaction(\n\t\texecute,\n\t\t|{}| {\n\t\t\t_ = execute(\"INSERT\") ? InsertFailed\n\t\t\tOk({})\n\t\t},\n\t)"))
			(line-break)
			(reflow "Here is the type I'm inferring. You will see")
			(reflow " ")
			(annotated code "<RecursiveType>")
			(reflow " ")
			(reflow "for parts of the type that repeat.")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[InsertFailed(<RecursiveType>)]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Recursive types are only allowed through nominal types.")
			(reflow " ")
			(reflow "If you need a recursive data structure, define a nominal type using")
			(reflow " ")
			(annotated code ":=")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Anonymous Recursion")
		(region (start 22 1) (end 22 41))
		(headline
			(reflow "I am inferring a recursive type that has no name somewhere in")
			(reflow " ")
			(annotated code "mirror_main")
			(reflow "."))
		(document
			(source-region (file "issue_11469_mirror_bare_try_in_higher_order_fn.md") (start 22 1) (end 22 41) (annotation error) (line-text "mirror_main = mirror_save(|_sql| Ok({}))"))
			(line-break)
			(reflow "Here is the type I'm inferring. You will see")
			(reflow " ")
			(annotated code "<RecursiveType>")
			(reflow " ")
			(reflow "for parts of the type that repeat.")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[InsertFailed(<RecursiveType>)]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Recursive types are only allowed through nominal types.")
			(reflow " ")
			(reflow "If you need a recursive data structure, define a nominal type using")
			(reflow " ")
			(annotated code ":=")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
OpBar,OpenCurly,CloseCurly,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,Comma,
CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "mirror_transaction"))
			(e-lambda
				(args
					(p-ident (raw "execute"))
					(p-ident (raw "operation")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "execute"))
									(e-string
										(e-string-part (raw "BEGIN"))))))
						(e-apply
							(e-ident (raw "operation"))
							(e-record))))))
		(s-decl
			(p-ident (raw "mirror_save"))
			(e-lambda
				(args
					(p-ident (raw "execute")))
				(e-apply
					(e-ident (raw "mirror_transaction"))
					(e-ident (raw "execute"))
					(e-lambda
						(args
							(p-record))
						(e-block
							(statements
								(s-decl
									(p-underscore)
									(e-binop (op "?")
										(e-apply
											(e-ident (raw "execute"))
											(e-string
												(e-string-part (raw "INSERT"))))
										(e-tag (raw "InsertFailed"))))
								(e-apply
									(e-tag (raw "Ok"))
									(e-record))))))))
		(s-decl
			(p-ident (raw "mirror_main"))
			(e-apply
				(e-ident (raw "mirror_save"))
				(e-lambda
					(args
						(p-ident (raw "_sql")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "mirror_transaction"))
		(e-lambda
			(args
				(p-assign (ident "execute"))
				(p-assign (ident "operation")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 287)
									(e-lookup-local
										(p-assign (ident "execute")))
									(e-string
										(e-literal (string "BEGIN")))))
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
				(e-call (constraint-fn-var 329)
					(e-lookup-local
						(p-assign (ident "operation")))
					(e-empty_record)))))
	(d-let
		(p-assign (ident "mirror_save"))
		(e-lambda
			(args
				(p-assign (ident "execute")))
			(e-call (constraint-fn-var 409)
				(e-lookup-local
					(p-assign (ident "mirror_transaction")))
				(e-lookup-local
					(p-assign (ident "execute")))
				(e-closure
					(captures
						(capture (ident "execute")))
					(e-lambda
						(args
							(p-record-destructure
								(destructs)))
						(e-block
							(s-let
								(p-underscore)
								(e-match
									(match
										(cond
											(e-call (constraint-fn-var 360)
												(e-lookup-local
													(p-assign (ident "execute")))
												(e-string
													(e-literal (string "INSERT")))))
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
																	(e-tag (name "InsertFailed")
																		(args
																			(e-lookup-local
																				(p-assign (ident "#err")))))))))))))))
							(e-tag (name "Ok")
								(args
									(e-empty_record)))))))))
	(d-let
		(p-assign (ident "mirror_main"))
		(e-call (constraint-fn-var 429)
			(e-runtime-error (tag "erroneous_value_use"))
			(e-lambda
				(args
					(p-assign (ident "_sql")))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a -> Try(ok, err)), ({} -> Try(ok, err)) -> Try(ok, err) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "(a -> Try(ok, err)), ({} -> Try(ok, err)) -> Try(ok, err) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
