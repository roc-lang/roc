# META
~~~ini
description=A derived list parser composes infallible and fallible format methods (issue 11246)
type=snippet
~~~
# SOURCE
~~~roc
Parser := {}.{
	State := { bytes : List(U8) }
	parse_u8 : Parser, State -> Try({ value : U8, rest : State }, _)
	parse_u8 = |_, _| { crash "unimplemented" }
	parse_list_start : Parser, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], _)
	parse_list_start = |_, _| { crash "unimplemented" }
	parse_list_next : Parser, State -> Try([Item(State), Done(State)], _)
	parse_list_next = |_, state| {
		match state.bytes {
			[] => Ok(Done(state))
			_ => Ok(Item(state))
		}
	}
	parse_list_after_item : Parser, State -> Try([Continue(State), Done(State)], _)
	parse_list_after_item = |_, _| { Err(UnexpectedByte) }
}
Rvn :: {}.{
	parse : List(U8) -> Try({ value : a, rest : Parser.State }, _)
		where [a.parser_for : Parser -> (Parser.State -> Try({ value : a, rest : Parser.State }, _))]
	parse = |bytes| {
		T : a
		parse_ = T.parser_for(Parser.{})
		parse_(Parser.State.{ bytes })
	}
}
expect {
	expected : List(U8)
	expected = []
	expected == Rvn.parse([])?.value
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,Comma,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,NoSpaceDotLowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
Underscore,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
CloseCurly,
UpperIdent,OpDoubleColon,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,CloseCurly,Comma,Underscore,CloseRound,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,OpenRound,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,CloseCurly,Comma,Underscore,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,Dot,OpenCurly,CloseCurly,CloseRound,
LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Dot,OpenCurly,LowerIdent,CloseCurly,CloseRound,
CloseCurly,
CloseCurly,
KwExpect,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpEquals,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,CloseSquare,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Parser")
				(args))
			(ty-record)
			(associated
				(s-type-decl
					(header (name "State")
						(args))
					(ty-record
						(anno-record-field (name "bytes")
							(ty-apply
								(ty (name "List"))
								(ty (name "U8"))))))
				(s-type-anno (name "parse_u8")
					(ty-fn
						(ty (name "Parser"))
						(ty (name "State"))
						(ty-apply
							(ty (name "Try"))
							(ty-record
								(anno-record-field (name "value")
									(ty (name "U8")))
								(anno-record-field (name "rest")
									(ty (name "State"))))
							(_))))
				(s-decl
					(p-ident (raw "parse_u8"))
					(e-lambda
						(args
							(p-underscore)
							(p-underscore))
						(e-block
							(statements
								(s-crash
									(e-string
										(e-string-part (raw "unimplemented"))))))))
				(s-type-anno (name "parse_list_start")
					(ty-fn
						(ty (name "Parser"))
						(ty (name "State"))
						(ty-apply
							(ty (name "Try"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Counted"))
										(ty-record
											(anno-record-field (name "len")
												(ty (name "U64")))
											(anno-record-field (name "rest")
												(ty (name "State")))))
									(ty-apply
										(ty (name "Uncounted"))
										(ty (name "State")))))
							(_))))
				(s-decl
					(p-ident (raw "parse_list_start"))
					(e-lambda
						(args
							(p-underscore)
							(p-underscore))
						(e-block
							(statements
								(s-crash
									(e-string
										(e-string-part (raw "unimplemented"))))))))
				(s-type-anno (name "parse_list_next")
					(ty-fn
						(ty (name "Parser"))
						(ty (name "State"))
						(ty-apply
							(ty (name "Try"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Item"))
										(ty (name "State")))
									(ty-apply
										(ty (name "Done"))
										(ty (name "State")))))
							(_))))
				(s-decl
					(p-ident (raw "parse_list_next"))
					(e-lambda
						(args
							(p-underscore)
							(p-ident (raw "state")))
						(e-block
							(statements
								(e-match
									(e-field-access
										(receiver
											(e-ident (raw "state")))
										(segment (mode "required") (field "bytes")))
									(branches
										(branch
											(p-list)
											(e-apply
												(e-tag (raw "Ok"))
												(e-apply
													(e-tag (raw "Done"))
													(e-ident (raw "state")))))
										(branch
											(p-underscore)
											(e-apply
												(e-tag (raw "Ok"))
												(e-apply
													(e-tag (raw "Item"))
													(e-ident (raw "state")))))))))))
				(s-type-anno (name "parse_list_after_item")
					(ty-fn
						(ty (name "Parser"))
						(ty (name "State"))
						(ty-apply
							(ty (name "Try"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Continue"))
										(ty (name "State")))
									(ty-apply
										(ty (name "Done"))
										(ty (name "State")))))
							(_))))
				(s-decl
					(p-ident (raw "parse_list_after_item"))
					(e-lambda
						(args
							(p-underscore)
							(p-underscore))
						(e-block
							(statements
								(e-apply
									(e-tag (raw "Err"))
									(e-tag (raw "UnexpectedByte")))))))))
		(s-type-decl
			(header (name "Rvn")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "parse")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty (name "U8")))
						(ty-apply
							(ty (name "Try"))
							(ty-record
								(anno-record-field (name "value")
									(ty-var (raw "a")))
								(anno-record-field (name "rest")
									(ty (name "Parser.State"))))
							(_)))
					(where
						(method (mod-of "a") (name "parser_for")
							(ty-fn
								(ty (name "Parser"))
								(ty-fn
									(ty (name "Parser.State"))
									(ty-apply
										(ty (name "Try"))
										(ty-record
											(anno-record-field (name "value")
												(ty-var (raw "a")))
											(anno-record-field (name "rest")
												(ty (name "Parser.State"))))
										(_)))))))
				(s-decl
					(p-ident (raw "parse"))
					(e-lambda
						(args
							(p-ident (raw "bytes")))
						(e-block
							(statements
								(s-type-decl
									(header (name "T")
										(args))
									(ty-var (raw "a")))
								(s-decl
									(p-ident (raw "parse_"))
									(e-apply
										(e-ident (raw "T.parser_for"))
										(e-nominal-record
											(mapper (e-tag (raw "Parser")))
											(backing (e-record)))))
								(e-apply
									(e-ident (raw "parse_"))
									(e-nominal-record
										(mapper (e-tag (raw "Parser.State")))
										(backing (e-record
												(field (field "bytes"))))))))))))
		(s-expect
			(e-block
				(statements
					(s-type-anno (name "expected")
						(ty-apply
							(ty (name "List"))
							(ty (name "U8"))))
					(s-decl
						(p-ident (raw "expected"))
						(e-list))
					(e-binop (op "==")
						(e-ident (raw "expected"))
						(e-field-access
							(receiver
								(e-question-suffix
									(e-apply
										(e-ident (raw "Rvn.parse"))
										(e-list))))
							(segment (mode "required") (field "value")))))))))
~~~
# FORMATTED
~~~roc
Parser := {}.{
	State := { bytes : List(U8) }
	parse_u8 : Parser, State -> Try({ value : U8, rest : State }, _)
	parse_u8 = |_, _| {
		crash "unimplemented"
	}
	parse_list_start : Parser, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], _)
	parse_list_start = |_, _| {
		crash "unimplemented"
	}
	parse_list_next : Parser, State -> Try([Item(State), Done(State)], _)
	parse_list_next = |_, state| {
		match state.bytes {
			[] => Ok(Done(state))
			_ => Ok(Item(state))
		}
	}
	parse_list_after_item : Parser, State -> Try([Continue(State), Done(State)], _)
	parse_list_after_item = |_, _| {
		Err(UnexpectedByte)
	}
}

Rvn :: {}.{
	parse : List(U8) -> Try({ value : a, rest : Parser.State }, _)
		where [a.parser_for : Parser -> (Parser.State -> Try({ value : a, rest : Parser.State }, _))]
	parse = |bytes| {
		T : a
		parse_ = T.parser_for(Parser.{})
		parse_(Parser.State.{ bytes })
	}
}
expect {
	expected : List(U8)
	expected = []
	expected == Rvn.parse([])?.value
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parser_for_derived_list_error_union_issue_11246.Parser.parse_u8"))
		(e-lambda
			(args
				(p-underscore)
				(p-underscore))
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-string
							(e-literal (string "unimplemented")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Parser") (local))
				(ty-lookup (name "State") (local))
				(ty-apply (name "Try") (builtin)
					(ty-record
						(field (field "value")
							(ty-lookup (name "U8") (builtin)))
						(field (field "rest")
							(ty-lookup (name "State") (local))))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "parser_for_derived_list_error_union_issue_11246.Parser.parse_list_start"))
		(e-lambda
			(args
				(p-underscore)
				(p-underscore))
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-string
							(e-literal (string "unimplemented")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Parser") (local))
				(ty-lookup (name "State") (local))
				(ty-apply (name "Try") (builtin)
					(ty-tag-union
						(ty-tag-name (name "Counted")
							(ty-record
								(field (field "len")
									(ty-lookup (name "U64") (builtin)))
								(field (field "rest")
									(ty-lookup (name "State") (local)))))
						(ty-tag-name (name "Uncounted")
							(ty-lookup (name "State") (local))))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "parser_for_derived_list_error_union_issue_11246.Parser.parse_list_next"))
		(e-lambda
			(args
				(p-underscore)
				(p-assign (ident "state")))
			(e-block
				(e-match
					(match
						(cond
							(e-field-access
								(receiver
									(e-lookup-local
										(p-assign (ident "state"))))
								(segments
									(segment (name "bytes") (mode "required")))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns))))
								(value
									(e-tag (name "Ok")
										(args
											(e-tag (name "Done")
												(args
													(e-lookup-local
														(p-assign (ident "state")))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-underscore)))
								(value
									(e-tag (name "Ok")
										(args
											(e-tag (name "Item")
												(args
													(e-lookup-local
														(p-assign (ident "state"))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Parser") (local))
				(ty-lookup (name "State") (local))
				(ty-apply (name "Try") (builtin)
					(ty-tag-union
						(ty-tag-name (name "Item")
							(ty-lookup (name "State") (local)))
						(ty-tag-name (name "Done")
							(ty-lookup (name "State") (local))))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "parser_for_derived_list_error_union_issue_11246.Parser.parse_list_after_item"))
		(e-lambda
			(args
				(p-underscore)
				(p-underscore))
			(e-block
				(e-tag (name "Err")
					(args
						(e-tag (name "UnexpectedByte"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Parser") (local))
				(ty-lookup (name "State") (local))
				(ty-apply (name "Try") (builtin)
					(ty-tag-union
						(ty-tag-name (name "Continue")
							(ty-lookup (name "State") (local)))
						(ty-tag-name (name "Done")
							(ty-lookup (name "State") (local))))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "parser_for_derived_list_error_union_issue_11246.Rvn.parse"))
		(e-lambda
			(args
				(p-assign (ident "bytes")))
			(e-block
				(s-type-var-alias (alias "T") (type-var "a")
					(ty-rigid-var (name "a")))
				(s-let
					(p-assign (ident "parse_"))
					(e-type-dispatch-call (method "parser_for") (type-dispatch-stmt 145) (constraint-fn-var 544)
						(args
							(e-nominal (nominal "Parser")
								(e-empty_record)))))
				(e-call (constraint-fn-var 563)
					(e-lookup-local
						(p-assign (ident "parse_")))
					(e-nominal (nominal "parser_for_derived_list_error_union_issue_11246.Parser.State")
						(e-record
							(fields
								(field (name "bytes")
									(e-lookup-local
										(p-assign (ident "bytes"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-record
						(field (field "value")
							(ty-rigid-var (name "a")))
						(field (field "rest")
							(ty-lookup (name "Parser.State") (local))))
					(ty-underscore)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "parser_for")
					(ty-fn (effectful false)
						(ty-lookup (name "Parser") (local))
						(ty-parens
							(ty-fn (effectful false)
								(ty-lookup (name "Parser.State") (local))
								(ty-apply (name "Try") (builtin)
									(ty-record
										(field (field "value")
											(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
										(field (field "rest")
											(ty-lookup (name "Parser.State") (local))))
									(ty-underscore)))))))))
	(s-nominal-decl
		(ty-header (name "Parser"))
		(ty-record))
	(s-nominal-decl
		(ty-header (name "parser_for_derived_list_error_union_issue_11246.Parser.State"))
		(ty-record
			(field (field "bytes")
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin))))))
	(s-nominal-decl
		(ty-header (name "Rvn"))
		(ty-record))
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "expected"))
				(e-empty_list))
			(e-method-eq (negated "false")
				(lhs
					(e-lookup-local
						(p-assign (ident "expected"))))
				(rhs
					(e-field-access
						(receiver
							(e-match
								(match
									(cond
										(e-call (constraint-fn-var 582)
											(e-lookup-local
												(p-assign (ident "parser_for_derived_list_error_union_issue_11246.Rvn.parse")))
											(e-empty_list)))
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
												(e-expect-err (snippet "Rvn.parse([])?")
													(e-lookup-local
														(p-assign (ident "#err"))))))))))
						(segments
							(segment (name "value") (mode "required")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Parser, Parser.State -> Try({ rest: Parser.State, value: U8 }, _b)"))
		(patt (type "Parser, Parser.State -> Try([Counted({ len: U64, rest: Parser.State }), Uncounted(Parser.State)], _b)"))
		(patt (type "Parser, Parser.State -> Try([Done(Parser.State), Item(Parser.State)], [])"))
		(patt (type "Parser, Parser.State -> Try([Continue(Parser.State), Done(Parser.State)], [UnexpectedByte, ..])"))
		(patt (type "List(U8) -> Try({ rest: Parser.State, value: a }, _b) where [a.parser_for : Parser -> (Parser.State -> Try({ rest: Parser.State, value: a }, _c))]")))
	(type_decls
		(nominal (type "Parser")
			(ty-header (name "Parser")))
		(nominal (type "Parser.State")
			(ty-header (name "parser_for_derived_list_error_union_issue_11246.Parser.State")))
		(nominal (type "Rvn")
			(ty-header (name "Rvn"))))
	(expressions
		(expr (type "Parser, Parser.State -> Try({ rest: Parser.State, value: U8 }, _b)"))
		(expr (type "Parser, Parser.State -> Try([Counted({ len: U64, rest: Parser.State }), Uncounted(Parser.State)], _b)"))
		(expr (type "Parser, Parser.State -> Try([Done(Parser.State), Item(Parser.State)], [])"))
		(expr (type "Parser, Parser.State -> Try([Continue(Parser.State), Done(Parser.State)], [UnexpectedByte, ..])"))
		(expr (type "List(U8) -> Try({ rest: Parser.State, value: a }, _b) where [a.parser_for : Parser -> (Parser.State -> Try({ rest: Parser.State, value: a }, _c))]"))))
~~~
