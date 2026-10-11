# META
~~~ini
description=A discarded binding that leaves a body-required where-clause output unpinned is rejected at check time instead of crashing at runtime (issues 9815, 9819)
type=file
~~~
# SOURCE
~~~roc
run = || {
    f = |_| Try.Err(NoMore)
    _ = Iter.collect(Iter.custom(0.U64, Unknown, f))
    Ok({})
}
~~~
# EXPECTED
TYPE NOT DETERMINED - static_dispatch_discarded_iter_issue_9815.md:3:9:3:53
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 3 9) (end 3 53))
		(headline
			(reflow "Nothing in this program determines the type this")
			(reflow " ")
			(annotated code "from_iter")
			(reflow " ")
			(reflow "method is called on:"))
		(document
			(source-region (file "static_dispatch_discarded_iter_issue_9815.md") (start 3 9) (end 3 53) (annotation error) (line-text "    _ = Iter.collect(Iter.custom(0.U64, Unknown, f))"))
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "from_iter")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
Underscore,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,Comma,UpperIdent,Comma,LowerIdent,CloseRound,CloseRound,
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
			(p-ident (raw "run"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-ident (raw "f"))
							(e-lambda
								(args
									(p-underscore))
								(e-apply
									(e-tag (raw "Try.Err"))
									(e-tag (raw "NoMore")))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "Iter.collect"))
								(e-apply
									(e-ident (raw "Iter.custom"))
									(e-typed-int (raw "0") (type "U64"))
									(e-tag (raw "Unknown"))
									(e-ident (raw "f")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
run = || {
	f = |_| Try.Err(NoMore)
	_ = Iter.collect(Iter.custom(0.U64, Unknown, f))
	Ok({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-assign (ident "f"))
					(e-lambda
						(args
							(p-underscore))
						(e-nominal-external
							(builtin)
							(e-tag (name "Err")
								(args
									(e-tag (name "NoMore")))))))
				(s-let
					(p-underscore)
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> [Ok({})]")))
	(expressions
		(expr (type "({}) -> [Ok({})]"))))
~~~
