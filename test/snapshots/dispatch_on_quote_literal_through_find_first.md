# META
~~~ini
description=Static dispatch chain on a find_first predicate's string-literal param resolves; the ?? default literal that previously orphaned the dispatch (when the capturing closure argument was prematurely generalized) no longer breaks it
type=snippet
~~~
# SOURCE
~~~roc
foo : U8 -> Str
foo = |letter| {
    val = ["hello", "world"].find_first(|letters| letters.to_utf8().contains(letter)) ?? ""
    val
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpDoubleQuestion,StringStart,StringPart,StringEnd,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "foo")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-lambda
				(args
					(p-ident (raw "letter")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "val"))
							(e-binop (op "??")
								(e-method-call (method ".find_first")
									(receiver
										(e-list
											(e-string
												(e-string-part (raw "hello")))
											(e-string
												(e-string-part (raw "world")))))
									(args
										(e-lambda
											(args
												(p-ident (raw "letters")))
											(e-method-call (method ".contains")
												(receiver
													(e-method-call (method ".to_utf8")
														(receiver
															(e-ident (raw "letters")))
														(args)))
												(args
													(e-ident (raw "letter")))))))
								(e-string
									(e-string-part (raw "")))))
						(e-ident (raw "val"))))))))
~~~
# FORMATTED
~~~roc
foo : U8 -> Str
foo = |letter| {
	val = ["hello", "world"].find_first(|letters| letters.to_utf8().contains(letter)) ?? ""
	val
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-lambda
			(args
				(p-assign (ident "letter")))
			(e-block
				(s-let
					(p-assign (ident "val"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "find_first") (constraint-fn-var 103)
									(receiver
										(e-list
											(elems
												(e-string
													(e-literal (string "hello")))
												(e-string
													(e-literal (string "world"))))))
									(args
										(e-closure
											(captures
												(capture (ident "letter")))
											(e-lambda
												(args
													(p-assign (ident "letters")))
												(e-dispatch-call (method "contains") (constraint-fn-var 101)
													(receiver
														(e-dispatch-call (method "to_utf8") (constraint-fn-var 99)
															(receiver
																(e-lookup-local
																	(p-assign (ident "letters"))))
															(args)))
													(args
														(e-lookup-local
															(p-assign (ident "letter"))))))))))
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
										(e-string
											(e-literal (string "")))))))))
				(e-lookup-local
					(p-assign (ident "val")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8 -> Str")))
	(expressions
		(expr (type "U8 -> Str"))))
~~~
