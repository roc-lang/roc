# META
~~~ini
description=A match pattern for a tag the matched value never has is reported as unmatchable (issue 12833)
type=file
~~~
# SOURCE
~~~roc
main! = |args| {
	letter = match args.first() {
		Ok("a") => A
		_ => B
	}
	match letter {
		C => {}
		_ => {}
	}
	Ok({})
}
~~~
# EXPECTED
UNMATCHABLE PATTERN - repro_issue_12833_incompatible_match_pattern.md:7:3:7:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 7 3) (end 7 4))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "can never match."))
		(document
			(source-region (file "repro_issue_12833_incompatible_match_pattern.md") (start 7 3) (end 7 4) (annotation warning) (line-text "\t\tC => {}"))
			(line-break)
			(reflow "The value this pattern matches never has the")
			(reflow " ")
			(annotated code "C")
			(reflow " ")
			(reflow "tag. Only")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "patterns mention it; nothing ever produces it.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "If")
			(reflow " ")
			(annotated code "C")
			(reflow " ")
			(reflow "should be possible here, add a type annotation to the value that lists it."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpFatArrow,UpperIdent,
Underscore,OpFatArrow,UpperIdent,
CloseCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,OpenCurly,CloseCurly,
Underscore,OpFatArrow,OpenCurly,CloseCurly,
CloseCurly,
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
						(s-decl
							(p-ident (raw "letter"))
							(e-match
								(e-method-call (method ".first")
									(receiver
										(e-ident (raw "args")))
									(args))
								(branches
									(branch
										(p-tag (raw "Ok")
											(p-string (raw """)
												(p-string-text (raw "a"))))
										(e-tag (raw "A")))
									(branch
										(p-underscore)
										(e-tag (raw "B"))))))
						(e-match
							(e-ident (raw "letter"))
							(branches
								(branch
									(p-tag (raw "C"))
									(e-record))
								(branch
									(p-underscore)
									(e-record))))
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
				(s-let
					(p-assign (ident "letter"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "first") (constraint-fn-var 260)
									(receiver
										(e-lookup-local
											(p-assign (ident "args"))))
									(args)))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "A"))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-tag (name "B"))))))))
				(s-expr
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "letter"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-empty_record)))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-empty_record)))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "a -> [Ok({})] where [a.first : a -> [Ok(b)], b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]), b.is_eq : b, b -> Bool]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "a -> [Ok({})] where [a.first : a -> [Ok(b)], b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]), b.is_eq : b, b -> Bool]"))))
~~~
