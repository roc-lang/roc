# META
~~~ini
description=Match patterns for tags no producer gives the matched value are reported as unmatchable
type=file
~~~
# SOURCE
~~~roc
pick = |b| if b A else B

pick_annotated : Bool -> [A, B]
pick_annotated = |b| if b A else B

mode = Debug

describe_mode = |_|
	match mode {
		Debug => "debug"
		Release => "release"
	}

main! = |args| {
	from_unannotated = match pick(args.is_empty()) {
		C => "c"
		_ => "other"
	}
	from_annotated = match pick_annotated(args.is_empty()) {
		C => "c"
		_ => "other"
	}
	wrapped = if args.is_empty() Wrap(A) else Wrap(B)
	from_payload = match wrapped {
		Wrap(C) => "c"
		_ => "other"
	}
	record = { kind: if args.is_empty() A else B }
	from_field = match record {
		{ kind: C } => "c"
		_ => "other"
	}
	alternatives = match pick(args.len() > 2) {
		A | C => "a or c"
		_ => "other"
	}
	echo!("${from_unannotated} ${from_annotated} ${from_payload} ${from_field} ${alternatives} ${describe_mode({})}")
	Ok({})
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - pattern_only_tag_warnings.md:9:8:9:12
UNMATCHABLE PATTERN - pattern_only_tag_warnings.md:11:3:11:10
UNMATCHABLE PATTERN - pattern_only_tag_warnings.md:16:3:16:4
UNMATCHABLE PATTERN - pattern_only_tag_warnings.md:20:3:20:4
UNMATCHABLE PATTERN - pattern_only_tag_warnings.md:25:3:25:10
UNMATCHABLE PATTERN - pattern_only_tag_warnings.md:30:3:30:14
UNMATCHABLE PATTERN - pattern_only_tag_warnings.md:34:7:34:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 9 8) (end 9 12))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "match value")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this match will always inspect the same value."))
		(document
			(source-region (file "pattern_only_tag_warnings.md") (start 9 8) (end 9 12) (annotation warning) (line-text "\tmatch mode {"))))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 11 3) (end 11 10))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "can never match."))
		(document
			(source-region (file "pattern_only_tag_warnings.md") (start 11 3) (end 11 10) (annotation warning) (line-text "\t\tRelease => \"release\""))
			(line-break)
			(reflow "The value this pattern matches never has the")
			(reflow " ")
			(annotated code "Release")
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
			(annotated code "Release")
			(reflow " ")
			(reflow "should be possible here, add a type annotation to the value that lists it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 16 3) (end 16 4))
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
			(source-region (file "pattern_only_tag_warnings.md") (start 16 3) (end 16 4) (annotation warning) (line-text "\t\tC => \"c\""))
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
			(reflow "should be possible here, add a type annotation to the value that lists it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 20 3) (end 20 4))
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
			(source-region (file "pattern_only_tag_warnings.md") (start 20 3) (end 20 4) (annotation warning) (line-text "\t\tC => \"c\""))
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
			(reflow "should be possible here, add a type annotation to the value that lists it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 25 3) (end 25 10))
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
			(source-region (file "pattern_only_tag_warnings.md") (start 25 3) (end 25 10) (annotation warning) (line-text "\t\tWrap(C) => \"c\""))
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
			(reflow "should be possible here, add a type annotation to the value that lists it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 30 3) (end 30 14))
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
			(source-region (file "pattern_only_tag_warnings.md") (start 30 3) (end 30 14) (annotation warning) (line-text "\t\t{ kind: C } => \"c\""))
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
			(reflow "should be possible here, add a type annotation to the value that lists it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 34 7) (end 34 8))
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
			(annotated keyword "match")
			(reflow " ")
			(reflow "can never match."))
		(document
			(source-region (file "pattern_only_tag_warnings.md") (start 34 7) (end 34 8) (annotation warning) (line-text "\t\tA | C => \"a or c\""))
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,UpperIdent,KwElse,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,UpperIdent,KwElse,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwIf,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,KwIf,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,UpperIdent,KwElse,UpperIdent,CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpGreaterThan,Int,CloseRound,OpenCurly,
UpperIdent,OpBar,UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
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
			(p-ident (raw "pick"))
			(e-lambda
				(args
					(p-ident (raw "b")))
				(e-if-then-else
					(e-ident (raw "b"))
					(e-tag (raw "A"))
					(e-tag (raw "B")))))
		(s-type-anno (name "pick_annotated")
			(ty-fn
				(ty (name "Bool"))
				(ty-tag-union
					(tags
						(ty (name "A"))
						(ty (name "B"))))))
		(s-decl
			(p-ident (raw "pick_annotated"))
			(e-lambda
				(args
					(p-ident (raw "b")))
				(e-if-then-else
					(e-ident (raw "b"))
					(e-tag (raw "A"))
					(e-tag (raw "B")))))
		(s-decl
			(p-ident (raw "mode"))
			(e-tag (raw "Debug")))
		(s-decl
			(p-ident (raw "describe_mode"))
			(e-lambda
				(args
					(p-underscore))
				(e-match
					(e-ident (raw "mode"))
					(branches
						(branch
							(p-tag (raw "Debug"))
							(e-string
								(e-string-part (raw "debug"))))
						(branch
							(p-tag (raw "Release"))
							(e-string
								(e-string-part (raw "release"))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "args")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "from_unannotated"))
							(e-match
								(e-apply
									(e-ident (raw "pick"))
									(e-method-call (method ".is_empty")
										(receiver
											(e-ident (raw "args")))
										(args)))
								(branches
									(branch
										(p-tag (raw "C"))
										(e-string
											(e-string-part (raw "c"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "other")))))))
						(s-decl
							(p-ident (raw "from_annotated"))
							(e-match
								(e-apply
									(e-ident (raw "pick_annotated"))
									(e-method-call (method ".is_empty")
										(receiver
											(e-ident (raw "args")))
										(args)))
								(branches
									(branch
										(p-tag (raw "C"))
										(e-string
											(e-string-part (raw "c"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "other")))))))
						(s-decl
							(p-ident (raw "wrapped"))
							(e-if-then-else
								(e-method-call (method ".is_empty")
									(receiver
										(e-ident (raw "args")))
									(args))
								(e-apply
									(e-tag (raw "Wrap"))
									(e-tag (raw "A")))
								(e-apply
									(e-tag (raw "Wrap"))
									(e-tag (raw "B")))))
						(s-decl
							(p-ident (raw "from_payload"))
							(e-match
								(e-ident (raw "wrapped"))
								(branches
									(branch
										(p-tag (raw "Wrap")
											(p-tag (raw "C")))
										(e-string
											(e-string-part (raw "c"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "other")))))))
						(s-decl
							(p-ident (raw "record"))
							(e-record
								(field (field "kind")
									(e-if-then-else
										(e-method-call (method ".is_empty")
											(receiver
												(e-ident (raw "args")))
											(args))
										(e-tag (raw "A"))
										(e-tag (raw "B"))))))
						(s-decl
							(p-ident (raw "from_field"))
							(e-match
								(e-ident (raw "record"))
								(branches
									(branch
										(p-record
											(field (name "kind") (rest false)
												(p-tag (raw "C"))))
										(e-string
											(e-string-part (raw "c"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "other")))))))
						(s-decl
							(p-ident (raw "alternatives"))
							(e-match
								(e-apply
									(e-ident (raw "pick"))
									(e-binop (op ">")
										(e-method-call (method ".len")
											(receiver
												(e-ident (raw "args")))
											(args))
										(e-int (raw "2"))))
								(branches
									(branch
										(p-alternatives
											(p-tag (raw "A"))
											(p-tag (raw "C")))
										(e-string
											(e-string-part (raw "a or c"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "other")))))))
						(e-apply
							(e-ident (raw "echo!"))
							(e-string
								(e-string-part (raw ""))
								(e-ident (raw "from_unannotated"))
								(e-string-part (raw " "))
								(e-ident (raw "from_annotated"))
								(e-string-part (raw " "))
								(e-ident (raw "from_payload"))
								(e-string-part (raw " "))
								(e-ident (raw "from_field"))
								(e-string-part (raw " "))
								(e-ident (raw "alternatives"))
								(e-string-part (raw " "))
								(e-apply
									(e-ident (raw "describe_mode"))
									(e-record))
								(e-string-part (raw ""))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
pick = |b| if (b) A else B

pick_annotated : Bool -> [A, B]
pick_annotated = |b| if (b) A else B

mode = Debug

describe_mode = |_|
	match mode {
		Debug => "debug"
		Release => "release"
	}

main! = |args| {
	from_unannotated = match pick(args.is_empty()) {
		C => "c"
		_ => "other"
	}
	from_annotated = match pick_annotated(args.is_empty()) {
		C => "c"
		_ => "other"
	}
	wrapped = if (args.is_empty()) Wrap(A) else Wrap(B)
	from_payload = match wrapped {
		Wrap(C) => "c"
		_ => "other"
	}
	record = { kind: if (args.is_empty()) A else B }
	from_field = match record {
		{ kind: C } => "c"
		_ => "other"
	}
	alternatives = match pick(args.len() > 2) {
		A | C => "a or c"
		_ => "other"
	}
	echo!("${from_unannotated} ${from_annotated} ${from_payload} ${from_field} ${alternatives} ${describe_mode({})}")
	Ok({})
}
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
		(p-assign (ident "pick"))
		(e-lambda
			(args
				(p-assign (ident "b")))
			(e-if
				(if-branches
					(if-branch
						(e-lookup-local
							(p-assign (ident "b")))
						(e-tag (name "A"))))
				(if-else
					(e-tag (name "B"))))))
	(d-let
		(p-assign (ident "pick_annotated"))
		(e-lambda
			(args
				(p-assign (ident "b")))
			(e-if
				(if-branches
					(if-branch
						(e-lookup-local
							(p-assign (ident "b")))
						(e-tag (name "A"))))
				(if-else
					(e-tag (name "B")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Bool") (builtin))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "mode"))
		(e-tag (name "Debug")))
	(d-let
		(p-assign (ident "describe_mode"))
		(e-lambda
			(args
				(p-underscore))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "mode"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "debug")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "release"))))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "args")))
			(e-block
				(s-let
					(p-assign (ident "from_unannotated"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 484)
									(e-lookup-local
										(p-assign (ident "pick")))
									(e-dispatch-call (method "is_empty") (constraint-fn-var 482)
										(receiver
											(e-lookup-local
												(p-assign (ident "args"))))
										(args))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "other")))))))))
				(s-let
					(p-assign (ident "from_annotated"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 510)
									(e-lookup-local
										(p-assign (ident "pick_annotated")))
									(e-dispatch-call (method "is_empty") (constraint-fn-var 508)
										(receiver
											(e-lookup-local
												(p-assign (ident "args"))))
										(args))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "other")))))))))
				(s-let
					(p-assign (ident "wrapped"))
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_empty") (constraint-fn-var 531)
									(receiver
										(e-lookup-local
											(p-assign (ident "args"))))
									(args))
								(e-tag (name "Wrap")
									(args
										(e-tag (name "A"))))))
						(if-else
							(e-tag (name "Wrap")
								(args
									(e-tag (name "B")))))))
				(s-let
					(p-assign (ident "from_payload"))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "wrapped"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "other")))))))))
				(s-let
					(p-assign (ident "record"))
					(e-record
						(fields
							(field (name "kind")
								(e-if
									(if-branches
										(if-branch
											(e-dispatch-call (method "is_empty") (constraint-fn-var 562)
												(receiver
													(e-lookup-local
														(p-assign (ident "args"))))
												(args))
											(e-tag (name "A"))))
									(if-else
										(e-tag (name "B"))))))))
				(s-let
					(p-assign (ident "from_field"))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "record"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-record-destructure
												(destructs
													(record-destruct (label "kind") (ident "kind")
														(sub-pattern
															(p-applied-tag)))))))
									(value
										(e-string
											(e-literal (string "c")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "other")))))))))
				(s-let
					(p-assign (ident "alternatives"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 611)
									(e-lookup-local
										(p-assign (ident "pick")))
									(e-dispatch-call (method "is_gt") (constraint-fn-var 609)
										(receiver
											(e-dispatch-call (method "len") (constraint-fn-var 599)
												(receiver
													(e-lookup-local
														(p-assign (ident "args"))))
												(args)))
										(args
											(e-num (value "2"))))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag))
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "a or c")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "other")))))))))
				(s-expr
					(e-call (constraint-fn-var 688)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-block
							(s-let
								(p-assign (ident "#interp_0"))
								(e-lookup-local
									(p-assign (ident "from_unannotated"))))
							(s-let
								(p-assign (ident "#interp_1"))
								(e-lookup-local
									(p-assign (ident "from_annotated"))))
							(s-let
								(p-assign (ident "#interp_2"))
								(e-lookup-local
									(p-assign (ident "from_payload"))))
							(s-let
								(p-assign (ident "#interp_3"))
								(e-lookup-local
									(p-assign (ident "from_field"))))
							(s-let
								(p-assign (ident "#interp_4"))
								(e-lookup-local
									(p-assign (ident "alternatives"))))
							(s-let
								(p-assign (ident "#interp_5"))
								(e-call (constraint-fn-var 644)
									(e-lookup-local
										(p-assign (ident "describe_mode")))
									(e-empty_record)))
							(e-interpolation (constraint-fn-var 665) (dispatcher-var 194)
								(first
									(e-literal (string "")))
								(parts
									(e-lookup-local
										(p-assign (ident "#interp_0")))
									(e-literal (string " "))
									(e-lookup-local
										(p-assign (ident "#interp_1")))
									(e-literal (string " "))
									(e-lookup-local
										(p-assign (ident "#interp_2")))
									(e-literal (string " "))
									(e-lookup-local
										(p-assign (ident "#interp_3")))
									(e-literal (string " "))
									(e-lookup-local
										(p-assign (ident "#interp_4")))
									(e-literal (string " "))
									(e-lookup-local
										(p-assign (ident "#interp_5")))
									(e-literal (string "")))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "Bool -> [A, B]"))
		(patt (type "Bool -> [A, B]"))
		(patt (type "[Debug, Release]"))
		(patt (type "_arg -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "a => [Ok({})] where [a.is_empty : a -> Bool, a.len : a -> c, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.is_gt : c, c -> Bool]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "Bool -> [A, B]"))
		(expr (type "Bool -> [A, B]"))
		(expr (type "[Debug, Release]"))
		(expr (type "_arg -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "a => [Ok({})] where [a.is_empty : a -> Bool, a.len : a -> c, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.is_gt : c, c -> Bool]"))))
~~~
