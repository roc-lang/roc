# META
~~~ini
description=A callback written as a call argument is judged by what its call gives it
type=file
~~~
# SOURCE
~~~roc
labels = List.map([A, B], |x|
	match x {
		A => "a"
		B => "b"
		C => "c"
	})

apply = |f, v| f(v)

applied = apply(
	|x|
		match x {
			A => 1
			C => 2
		},
	A,
)

feed_c = |f| f(C)

fed = feed_c(|x|
	match x {
		A => 1
		C => 2
	})

annotated : ([A, C] -> U64) -> U64
annotated = |f| f(A)

from_annotation = annotated(|x|
	match x {
		A => 1
		C => 2
	})

forward = |items|
	List.map(items, |x|
		match x {
			A => 1
			C => 2
		})

method_callback = |_| {
	xs = [A, B]
	xs.map(|v|
		match v {
			C => 1
			_ => 0
		})
}

nested = |v|
	apply(
		|x|
			match x {
				A => 1
				C => 2
			},
		v,
	)
~~~
# EXPECTED
UNMATCHABLE PATTERN - pattern_only_tag_callback_argument.md:5:3:5:4
UNMATCHABLE PATTERN - pattern_only_tag_callback_argument.md:14:4:14:5
UNMATCHABLE PATTERN - pattern_only_tag_callback_argument.md:23:3:23:4
UNMATCHABLE PATTERN - pattern_only_tag_callback_argument.md:47:4:47:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 5 3) (end 5 4))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "third")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "can never match."))
		(document
			(source-region (file "pattern_only_tag_callback_argument.md") (start 5 3) (end 5 4) (annotation warning) (line-text "\t\tC => \"c\""))
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
		(region (start 14 4) (end 14 5))
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
			(source-region (file "pattern_only_tag_callback_argument.md") (start 14 4) (end 14 5) (annotation warning) (line-text "\t\t\tC => 2"))
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
		(region (start 23 3) (end 23 4))
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
			(source-region (file "pattern_only_tag_callback_argument.md") (start 23 3) (end 23 4) (annotation warning) (line-text "\t\tA => 1"))
			(line-break)
			(reflow "The value this pattern matches never has the")
			(reflow " ")
			(annotated code "A")
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
			(annotated code "A")
			(reflow " ")
			(reflow "should be possible here, add a type annotation to the value that lists it.")))
	(report
		(severity warning)
		(title "Unmatchable Pattern")
		(region (start 47 4) (end 47 5))
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
			(source-region (file "pattern_only_tag_callback_argument.md") (start 47 4) (end 47 5) (annotation warning) (line-text "\t\t\tC => 1"))
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
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Comma,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,Comma,
UpperIdent,Comma,
CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,CloseRound,
LowerIdent,OpColon,OpenRound,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
LowerIdent,NoSpaceOpenRound,
OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,Comma,
LowerIdent,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "labels"))
			(e-apply
				(e-ident (raw "List.map"))
				(e-list
					(e-tag (raw "A"))
					(e-tag (raw "B")))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-match
						(e-ident (raw "x"))
						(branches
							(branch
								(p-tag (raw "A"))
								(e-string
									(e-string-part (raw "a"))))
							(branch
								(p-tag (raw "B"))
								(e-string
									(e-string-part (raw "b"))))
							(branch
								(p-tag (raw "C"))
								(e-string
									(e-string-part (raw "c")))))))))
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "f"))
					(p-ident (raw "v")))
				(e-apply
					(e-ident (raw "f"))
					(e-ident (raw "v")))))
		(s-decl
			(p-ident (raw "applied"))
			(e-apply
				(e-ident (raw "apply"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-match
						(e-ident (raw "x"))
						(branches
							(branch
								(p-tag (raw "A"))
								(e-int (raw "1")))
							(branch
								(p-tag (raw "C"))
								(e-int (raw "2"))))))
				(e-tag (raw "A"))))
		(s-decl
			(p-ident (raw "feed_c"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-tag (raw "C")))))
		(s-decl
			(p-ident (raw "fed"))
			(e-apply
				(e-ident (raw "feed_c"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-match
						(e-ident (raw "x"))
						(branches
							(branch
								(p-tag (raw "A"))
								(e-int (raw "1")))
							(branch
								(p-tag (raw "C"))
								(e-int (raw "2"))))))))
		(s-type-anno (name "annotated")
			(ty-fn
				(ty-fn
					(ty-tag-union
						(tags
							(ty (name "A"))
							(ty (name "C"))))
					(ty (name "U64")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "annotated"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-tag (raw "A")))))
		(s-decl
			(p-ident (raw "from_annotation"))
			(e-apply
				(e-ident (raw "annotated"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-match
						(e-ident (raw "x"))
						(branches
							(branch
								(p-tag (raw "A"))
								(e-int (raw "1")))
							(branch
								(p-tag (raw "C"))
								(e-int (raw "2"))))))))
		(s-decl
			(p-ident (raw "forward"))
			(e-lambda
				(args
					(p-ident (raw "items")))
				(e-apply
					(e-ident (raw "List.map"))
					(e-ident (raw "items"))
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "A"))
									(e-int (raw "1")))
								(branch
									(p-tag (raw "C"))
									(e-int (raw "2")))))))))
		(s-decl
			(p-ident (raw "method_callback"))
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
			(p-ident (raw "nested"))
			(e-lambda
				(args
					(p-ident (raw "v")))
				(e-apply
					(e-ident (raw "apply"))
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "A"))
									(e-int (raw "1")))
								(branch
									(p-tag (raw "C"))
									(e-int (raw "2"))))))
					(e-ident (raw "v")))))))
~~~
# FORMATTED
~~~roc
labels = List.map(
	[A, B],
	|x|
		match x {
			A => "a"
			B => "b"
			C => "c"
		},
)

apply = |f, v| f(v)

applied = apply(
	|x|
		match x {
			A => 1
			C => 2
		},
	A,
)

feed_c = |f| f(C)

fed = feed_c(
	|x|
		match x {
			A => 1
			C => 2
		},
)

annotated : ([A, C] -> U64) -> U64
annotated = |f| f(A)

from_annotation = annotated(
	|x|
		match x {
			A => 1
			C => 2
		},
)

forward = |items|
	List.map(
		items,
		|x|
			match x {
				A => 1
				C => 2
			},
	)

method_callback = |_| {
	xs = [A, B]
	xs.map(
		|v|
			match v {
				C => 1
				_ => 0
			},
	)
}

nested = |v|
	apply(
		|x|
			match x {
				A => 1
				C => 2
			},
		v,
	)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "labels"))
		(e-call (constraint-fn-var 414)
			(e-lookup-external
				(builtin))
			(e-list
				(elems
					(e-tag (name "A"))
					(e-tag (name "B"))))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "b")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "c")))))))))))
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "f"))
				(p-assign (ident "v")))
			(e-call (constraint-fn-var 418)
				(e-lookup-local
					(p-assign (ident "f")))
				(e-lookup-local
					(p-assign (ident "v"))))))
	(d-let
		(p-assign (ident "applied"))
		(e-call (constraint-fn-var 433)
			(e-lookup-local
				(p-assign (ident "apply")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
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
									(e-num (value "2"))))))))
			(e-tag (name "A"))))
	(d-let
		(p-assign (ident "feed_c"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call (constraint-fn-var 438)
				(e-lookup-local
					(p-assign (ident "f")))
				(e-tag (name "C")))))
	(d-let
		(p-assign (ident "fed"))
		(e-call (constraint-fn-var 453)
			(e-lookup-local
				(p-assign (ident "feed_c")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
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
									(e-num (value "2"))))))))))
	(d-let
		(p-assign (ident "annotated"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call (constraint-fn-var 464)
				(e-lookup-local
					(p-assign (ident "f")))
				(e-tag (name "A"))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-tag-union
							(ty-tag-name (name "A"))
							(ty-tag-name (name "C")))
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "from_annotation"))
		(e-call (constraint-fn-var 477)
			(e-lookup-local
				(p-assign (ident "annotated")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
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
									(e-num (value "2"))))))))))
	(d-let
		(p-assign (ident "forward"))
		(e-lambda
			(args
				(p-assign (ident "items")))
			(e-call (constraint-fn-var 492)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "items")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "x"))))
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
										(e-num (value "2")))))))))))
	(d-let
		(p-assign (ident "method_callback"))
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
				(e-dispatch-call (method "map") (constraint-fn-var 514)
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
		(p-assign (ident "nested"))
		(e-lambda
			(args
				(p-assign (ident "v")))
			(e-call (constraint-fn-var 554)
				(e-lookup-local
					(p-assign (ident "apply")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "x"))))
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
										(e-num (value "2"))))))))
				(e-lookup-local
					(p-assign (ident "v")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Str)"))
		(patt (type "(c -> d), c -> d"))
		(patt (type "Dec"))
		(patt (type "([C] -> c) -> c"))
		(patt (type "Dec"))
		(patt (type "([A, C] -> U64) -> U64"))
		(patt (type "U64"))
		(patt (type "List([A, C]) -> List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "[A, C] -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "List(Str)"))
		(expr (type "(c -> d), c -> d"))
		(expr (type "Dec"))
		(expr (type "([C] -> c) -> c"))
		(expr (type "Dec"))
		(expr (type "([A, C] -> U64) -> U64"))
		(expr (type "U64"))
		(expr (type "List([A, C]) -> List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "[A, C] -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))))
~~~
