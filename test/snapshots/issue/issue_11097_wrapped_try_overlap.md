# META
~~~ini
description=A propagated tag that repeats an inferred wrapped try row tag relates its payload, which here is an anonymous recursive type
type=snippet
~~~
# SOURCE
~~~roc
run = |save| {
	_ = save({})?
	_ = save({}) ? PersistFailed
	Ok({})
}

use = run(|_| Err(PersistFailed(Foo)))
~~~
# EXPECTED
ANONYMOUS RECURSION - issue_11097_wrapped_try_overlap.md:7:1:7:39
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Anonymous Recursion")
		(region (start 7 1) (end 7 39))
		(headline
			(reflow "I am inferring a recursive type that has no name somewhere in")
			(reflow " ")
			(annotated code "use")
			(reflow "."))
		(document
			(source-region (file "issue_11097_wrapped_try_overlap.md") (start 7 1) (end 7 39) (annotation error) (line-text "use = run(|_| Err(PersistFailed(Foo)))"))
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
			(text "[Foo, PersistFailed(<RecursiveType>)]")
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "save")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "save"))
									(e-record))))
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "save"))
									(e-record))
								(e-tag (raw "PersistFailed"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "use"))
			(e-apply
				(e-ident (raw "run"))
				(e-lambda
					(args
						(p-underscore))
					(e-apply
						(e-tag (raw "Err"))
						(e-apply
							(e-tag (raw "PersistFailed"))
							(e-tag (raw "Foo")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 262)
									(e-lookup-local
										(p-assign (ident "save")))
									(e-empty_record)))
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
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 301)
									(e-lookup-local
										(p-assign (ident "save")))
									(e-empty_record)))
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
														(e-tag (name "PersistFailed")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "use"))
		(e-call (constraint-fn-var 365)
			(e-lookup-local
				(p-assign (ident "run")))
			(e-lambda
				(args
					(p-underscore))
				(e-tag (name "Err")
					(args
						(e-tag (name "PersistFailed")
							(args
								(e-tag (name "Foo"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({} -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..err])"))
		(patt (type "Error")))
	(expressions
		(expr (type "({} -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..err])"))
		(expr (type "Error"))))
~~~
