# META
~~~ini
description=Mutually recursive methods annotated with _ holes where one body pins its hole and the other member's where-clause method uses it (issue 11605)
type=file
~~~
# SOURCE
~~~roc
Client(effects) := { effects : effects }.{
    first : Client(_), List(_), a -> Try(Str, [Refused(Str), ..others]) where [a.to_str : a -> Str]
    first = |client, items, x| if List.len(items) == 0 Ok(x.to_str()) else second(client, items, x)

    second : Client(_), List(_), b -> Try(Str, [Refused(Str), ..others]) where [b.to_str : b -> Str]
    second = |client, items, y| {
        pinned : List(Str)
        pinned = items
        first(client, List.drop_first(pinned, 1), y)
    }
}

run : Str -> Try(Str, [Refused(Str)])
run = |sql| {
    client = Client.{ effects: {} }
    done = Client.first(client, [sql], 1.U64)?
    Ok(done)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,KwIf,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpEquals,Int,UpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,Comma,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,LowerIdent,CloseSquare,Comma,Int,NoSpaceDotUpperIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Client")
				(args
					(ty-var (raw "effects"))))
			(ty-record
				(anno-record-field (name "effects")
					(ty-var (raw "effects"))))
			(associated
				(s-type-anno (name "first")
					(ty-fn
						(ty-apply
							(ty (name "Client"))
							(_))
						(ty-apply
							(ty (name "List"))
							(_))
						(ty-var (raw "a"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Refused"))
										(ty (name "Str"))))
								(ty-var (raw "others")))))
					(where
						(method (mod-of "a") (name "to_str")
							(ty-fn
								(ty-var (raw "a"))
								(ty (name "Str"))))))
				(s-decl
					(p-ident (raw "first"))
					(e-lambda
						(args
							(p-ident (raw "client"))
							(p-ident (raw "items"))
							(p-ident (raw "x")))
						(e-if-then-else
							(e-binop (op "==")
								(e-apply
									(e-ident (raw "List.len"))
									(e-ident (raw "items")))
								(e-int (raw "0")))
							(e-apply
								(e-tag (raw "Ok"))
								(e-method-call (method ".to_str")
									(receiver
										(e-ident (raw "x")))
									(args)))
							(e-apply
								(e-ident (raw "second"))
								(e-ident (raw "client"))
								(e-ident (raw "items"))
								(e-ident (raw "x"))))))
				(s-type-anno (name "second")
					(ty-fn
						(ty-apply
							(ty (name "Client"))
							(_))
						(ty-apply
							(ty (name "List"))
							(_))
						(ty-var (raw "b"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Refused"))
										(ty (name "Str"))))
								(ty-var (raw "others")))))
					(where
						(method (mod-of "b") (name "to_str")
							(ty-fn
								(ty-var (raw "b"))
								(ty (name "Str"))))))
				(s-decl
					(p-ident (raw "second"))
					(e-lambda
						(args
							(p-ident (raw "client"))
							(p-ident (raw "items"))
							(p-ident (raw "y")))
						(e-block
							(statements
								(s-type-anno (name "pinned")
									(ty-apply
										(ty (name "List"))
										(ty (name "Str"))))
								(s-decl
									(p-ident (raw "pinned"))
									(e-ident (raw "items")))
								(e-apply
									(e-ident (raw "first"))
									(e-ident (raw "client"))
									(e-apply
										(e-ident (raw "List.drop_first"))
										(e-ident (raw "pinned"))
										(e-int (raw "1")))
									(e-ident (raw "y")))))))))
		(s-type-anno (name "run")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Refused"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "sql")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "client"))
							(e-nominal-record
								(mapper (e-tag (raw "Client")))
								(backing (e-record
										(field (field "effects")
											(e-record))))))
						(s-decl
							(p-ident (raw "done"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "Client.first"))
									(e-ident (raw "client"))
									(e-list
										(e-ident (raw "sql")))
									(e-typed-int (raw "1") (type "U64")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "done")))))))))
~~~
# FORMATTED
~~~roc
Client(effects) := { effects : effects }.{
	first : Client(_), List(_), a -> Try(Str, [Refused(Str), ..others]) where [a.to_str : a -> Str]
	first = |client, items, x| if List.len(items) == 0 Ok(x.to_str()) else second(client, items, x)

	second : Client(_), List(_), b -> Try(Str, [Refused(Str), ..others]) where [b.to_str : b -> Str]
	second = |client, items, y| {
		pinned : List(Str)
		pinned = items
		first(client, List.drop_first(pinned, 1), y)
	}
}

run : Str -> Try(Str, [Refused(Str)])
run = |sql| {
	client = Client.{ effects: {} }
	done = Client.first(client, [sql], 1.U64)?
	Ok(done)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_issue_11605.Client.first"))
		(e-lambda
			(args
				(p-assign (ident "client"))
				(p-assign (ident "items"))
				(p-assign (ident "x")))
			(e-if
				(if-branches
					(if-branch
						(e-method-eq (negated "false")
							(lhs
								(e-call (constraint-fn-var 422)
									(e-lookup-external
										(builtin))
									(e-lookup-local
										(p-assign (ident "items")))))
							(rhs
								(e-num (value "0"))))
						(e-tag (name "Ok")
							(args
								(e-dispatch-call (method "to_str") (constraint-fn-var 440)
									(receiver
										(e-lookup-local
											(p-assign (ident "x"))))
									(args))))))
				(if-else
					(e-call (constraint-fn-var 454)
						(e-lookup-local
							(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_issue_11605.Client.second")))
						(e-lookup-local
							(p-assign (ident "client")))
						(e-lookup-local
							(p-assign (ident "items")))
						(e-lookup-local
							(p-assign (ident "x")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Client") (local)
					(ty-underscore))
				(ty-apply (name "List") (builtin)
					(ty-underscore))
				(ty-rigid-var (name "a"))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin)))
						(ty-rigid-var (name "others")))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Str") (builtin)))))))
	(d-let
		(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_issue_11605.Client.second"))
		(e-lambda
			(args
				(p-assign (ident "client"))
				(p-assign (ident "items"))
				(p-assign (ident "y")))
			(e-block
				(s-let
					(p-assign (ident "pinned"))
					(e-lookup-local
						(p-assign (ident "items"))))
				(e-call (constraint-fn-var 506)
					(e-lookup-local
						(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_issue_11605.Client.first")))
					(e-lookup-local
						(p-assign (ident "client")))
					(e-call (constraint-fn-var 505)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "pinned")))
						(e-num (value "1")))
					(e-lookup-local
						(p-assign (ident "y"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Client") (local)
					(ty-underscore))
				(ty-apply (name "List") (builtin)
					(ty-underscore))
				(ty-rigid-var (name "b"))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin)))
						(ty-rigid-var (name "others")))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "to_str")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))
						(ty-lookup (name "Str") (builtin)))))))
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "sql")))
			(e-block
				(s-let
					(p-assign (ident "client"))
					(e-nominal (nominal "Client")
						(e-record
							(fields
								(field (name "effects")
									(e-empty_record))))))
				(s-let
					(p-assign (ident "done"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 556)
									(e-lookup-local
										(p-assign (ident "mutual_recursive_methods_underscore_hole_pinned_issue_11605.Client.first")))
									(e-lookup-local
										(p-assign (ident "client")))
									(e-list
										(elems
											(e-lookup-local
												(p-assign (ident "sql")))))
									(e-typed-int (value "1") (type "U64"))))
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
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "done")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Refused")
							(ty-lookup (name "Str") (builtin))))))))
	(s-nominal-decl
		(ty-header (name "Client")
			(ty-args
				(ty-rigid-var (name "effects"))))
		(ty-record
			(field (field "effects")
				(ty-rigid-var-lookup (ty-rigid-var (name "effects")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Client(_c), List(Str), a -> Try(Str, [Refused(Str), ..others]) where [a.to_str : a -> Str]"))
		(patt (type "Client(_c), List(Str), b -> Try(Str, [Refused(Str), ..others]) where [b.to_str : b -> Str]"))
		(patt (type "Str -> Try(Str, [Refused(Str)])")))
	(type_decls
		(nominal (type "Client(effects)")
			(ty-header (name "Client")
				(ty-args
					(ty-rigid-var (name "effects"))))))
	(expressions
		(expr (type "Client(_c), List(Str), a -> Try(Str, [Refused(Str), ..others]) where [a.to_str : a -> Str]"))
		(expr (type "Client(_c), List(Str), b -> Try(Str, [Refused(Str), ..others]) where [b.to_str : b -> Str]"))
		(expr (type "Str -> Try(Str, [Refused(Str)])"))))
~~~
