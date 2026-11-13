# META
~~~ini
description=Match patterns for tags a producer may give the matched value are not reported
type=file
~~~
# SOURCE
~~~roc
exhaustive_param = |v|
	match v {
		A => 1
		C => 2
	}

mixed = |c, v| {
	w = if c v else A
	match w {
		C => 1
		_ => 2
	}
}

call_param = |f|
	match f({}) {
		C => 1
		_ => 2
	}

countdown = |n|
	if n == 0 {
		A
	} else {
		match countdown(n - 1) {
			C => B
			_ => C
		}
	}

kind_of = |x|
	match x.kind() {
		C => 1
		_ => 2
	}

mode : [Debug, Release]
mode = Debug

describe_mode = |_|
	match mode {
		Debug => "debug"
		Release => "release"
	}

inline_callback = |_| {
	xs = [A, B]
	xs.map(|v|
		match v {
			C => 1
			_ => 0
		})
}

main! = |_args| {
	echo!(describe_mode({}))
	Ok({})
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - pattern_only_tag_produced_no_warning.md:41:8:41:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 41 8) (end 41 12))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "match value")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this match will always inspect the same value."))
		(document
			(source-region (file "pattern_only_tag_produced_no_warning.md") (start 41 8) (end 41 12) (annotation warning) (line-text "\tmatch mode {")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwIf,LowerIdent,LowerIdent,KwElse,UpperIdent,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwIf,LowerIdent,OpEquals,Int,OpenCurly,
UpperIdent,
CloseCurly,KwElse,OpenCurly,
KwMatch,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,
Underscore,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseRound,
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
			(p-ident (raw "exhaustive_param"))
			(e-lambda
				(args
					(p-ident (raw "v")))
				(e-match
					(e-ident (raw "v"))
					(branches
						(branch
							(p-tag (raw "A"))
							(e-int (raw "1")))
						(branch
							(p-tag (raw "C"))
							(e-int (raw "2")))))))
		(s-decl
			(p-ident (raw "mixed"))
			(e-lambda
				(args
					(p-ident (raw "c"))
					(p-ident (raw "v")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "w"))
							(e-if-then-else
								(e-ident (raw "c"))
								(e-ident (raw "v"))
								(e-tag (raw "A"))))
						(e-match
							(e-ident (raw "w"))
							(branches
								(branch
									(p-tag (raw "C"))
									(e-int (raw "1")))
								(branch
									(p-underscore)
									(e-int (raw "2")))))))))
		(s-decl
			(p-ident (raw "call_param"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-match
					(e-apply
						(e-ident (raw "f"))
						(e-record))
					(branches
						(branch
							(p-tag (raw "C"))
							(e-int (raw "1")))
						(branch
							(p-underscore)
							(e-int (raw "2")))))))
		(s-decl
			(p-ident (raw "countdown"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "n"))
						(e-int (raw "0")))
					(e-block
						(statements
							(e-tag (raw "A"))))
					(e-block
						(statements
							(e-match
								(e-apply
									(e-ident (raw "countdown"))
									(e-binop (op "-")
										(e-ident (raw "n"))
										(e-int (raw "1"))))
								(branches
									(branch
										(p-tag (raw "C"))
										(e-tag (raw "B")))
									(branch
										(p-underscore)
										(e-tag (raw "C"))))))))))
		(s-decl
			(p-ident (raw "kind_of"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-match
					(e-method-call (method ".kind")
						(receiver
							(e-ident (raw "x")))
						(args))
					(branches
						(branch
							(p-tag (raw "C"))
							(e-int (raw "1")))
						(branch
							(p-underscore)
							(e-int (raw "2")))))))
		(s-type-anno (name "mode")
			(ty-tag-union
				(tags
					(ty (name "Debug"))
					(ty (name "Release")))))
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
			(p-ident (raw "inline_callback"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "xs"))
							(e-list
								(e-tag (raw "A"))
								(e-tag (raw "B"))))
						(e-method-call (method ".map")
							(receiver
								(e-ident (raw "xs")))
							(args
								(e-lambda
									(args
										(p-ident (raw "v")))
									(e-match
										(e-ident (raw "v"))
										(branches
											(branch
												(p-tag (raw "C"))
												(e-int (raw "1")))
											(branch
												(p-underscore)
												(e-int (raw "0"))))))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(e-apply
							(e-ident (raw "echo!"))
							(e-apply
								(e-ident (raw "describe_mode"))
								(e-record)))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
exhaustive_param = |v|
	match v {
		A => 1
		C => 2
	}

mixed = |c, v| {
	w = if (c) v else A
	match w {
		C => 1
		_ => 2
	}
}

call_param = |f|
	match f({}) {
		C => 1
		_ => 2
	}

countdown = |n|
	if n == 0 {
		A
	} else {
		match countdown(n - 1) {
			C => B
			_ => C
		}
	}

kind_of = |x|
	match x.kind() {
		C => 1
		_ => 2
	}

mode : [Debug, Release]
mode = Debug

describe_mode = |_|
	match mode {
		Debug => "debug"
		Release => "release"
	}

inline_callback = |_| {
	xs = [A, B]
	xs.map(
		|v|
			match v {
				C => 1
				_ => 0
			},
	)
}

main! = |_args| {
	echo!(describe_mode({}))
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
		(p-assign (ident "exhaustive_param"))
		(e-lambda
			(args
				(p-assign (ident "v")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "v"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "2")))))))))
	(d-let
		(p-assign (ident "mixed"))
		(e-lambda
			(args
				(p-assign (ident "c"))
				(p-assign (ident "v")))
			(e-block
				(s-let
					(p-assign (ident "w"))
					(e-if
						(if-branches
							(if-branch
								(e-lookup-local
									(p-assign (ident "c")))
								(e-lookup-local
									(p-assign (ident "v")))))
						(if-else
							(e-tag (name "A")))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "w"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-num (value "1"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-underscore)))
								(value
									(e-num (value "2"))))))))))
	(d-let
		(p-assign (ident "call_param"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-match
				(match
					(cond
						(e-call (constraint-fn-var 429)
							(e-lookup-local
								(p-assign (ident "f")))
							(e-empty_record)))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "2")))))))))
	(d-let
		(p-assign (ident "countdown"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-if
				(if-branches
					(if-branch
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "n"))))
							(rhs
								(e-num (value "0"))))
						(e-block
							(e-tag (name "A")))))
				(if-else
					(e-block
						(e-match
							(match
								(cond
									(e-call (constraint-fn-var 469)
										(e-lookup-local
											(p-assign (ident "countdown")))
										(e-dispatch-call (method "minus") (constraint-fn-var 467)
											(receiver
												(e-lookup-local
													(p-assign (ident "n"))))
											(args
												(e-num (value "1"))))))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-tag (name "B"))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-underscore)))
										(value
											(e-tag (name "C"))))))))))))
	(d-let
		(p-assign (ident "kind_of"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-match
				(match
					(cond
						(e-dispatch-call (method "kind") (constraint-fn-var 481)
							(receiver
								(e-lookup-local
									(p-assign (ident "x"))))
							(args)))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "2")))))))))
	(d-let
		(p-assign (ident "mode"))
		(e-tag (name "Debug"))
		(annotation
			(ty-tag-union
				(ty-tag-name (name "Debug"))
				(ty-tag-name (name "Release")))))
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
		(p-assign (ident "inline_callback"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "xs"))
					(e-list
						(elems
							(e-tag (name "A"))
							(e-tag (name "B")))))
				(e-dispatch-call (method "map") (constraint-fn-var 532)
					(receiver
						(e-lookup-local
							(p-assign (ident "xs"))))
					(args
						(e-lambda
							(args
								(p-assign (ident "v")))
							(e-match
								(match
									(cond
										(e-lookup-local
											(p-assign (ident "v"))))
									(branches
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-num (value "1"))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-underscore)))
											(value
												(e-num (value "0")))))))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-expr
					(e-call (constraint-fn-var 577)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-call (constraint-fn-var 576)
							(e-lookup-local
								(p-assign (ident "describe_mode")))
							(e-empty_record))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "[A, C] -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "Bool, [A, C, ..] -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "({} -> [C, ..]) -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "d -> [A, B, C] where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), d.is_eq : d, d -> Bool, d.minus : d, e -> d, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "d -> e where [d.kind : d -> [C], e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(patt (type "[Debug, Release]"))
		(patt (type "_arg -> d where [d.from_quote : Str -> Try(d, [BadQuotedBytes(Str)])]"))
		(patt (type "_arg -> List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "_arg => [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "[A, C] -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "Bool, [A, C, ..] -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "({} -> [C, ..]) -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "d -> [A, B, C] where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), d.is_eq : d, d -> Bool, d.minus : d, e -> d, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "d -> e where [d.kind : d -> [C], e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]"))
		(expr (type "[Debug, Release]"))
		(expr (type "_arg -> d where [d.from_quote : Str -> Try(d, [BadQuotedBytes(Str)])]"))
		(expr (type "_arg -> List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "_arg => [Ok({})]"))))
~~~
