# META
~~~ini
description=An ellipsis statement on its own line after statements ending in various kinds of expressions (issue 12219)
type=snippet
~~~
# SOURCE
~~~roc
after_ident = |args| {
    _ = args
    ...
}

after_call = |f| {
    _ = f(1)
    ...
}

after_field = |r| {
    _ = r.a
    ...
}

after_tuple_access = |_| {
    _ = (1, 2).0
    ...
}

after_question = |r| {
    _ = r?
    ...
}

after_tag = |_| {
    _ = Ok(1)
    ...
}

after_string = |_| {
    _ = "hi"
    ...
}

after_int = |_| {
    _ = 1
    ...
}

after_list = |_| {
    _ = [1, 2]
    ...
}

after_record = |_| {
    _ = { a: 1 }
    ...
}

after_dbg = |x| {
    dbg x
    ...
}

after_ellipsis = |_| {
    ...
    ...
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,OpenRound,Int,Comma,Int,CloseRound,NoSpaceDotInt,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpQuestion,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,StringStart,StringPart,StringEnd,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,Int,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,OpenSquare,Int,Comma,Int,CloseSquare,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwDbg,LowerIdent,
TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
TripleDot,
TripleDot,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "after_ident"))
			(e-lambda
				(args
					(p-ident (raw "args")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-ident (raw "args")))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_call"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "f"))
								(e-int (raw "1"))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_field"))
			(e-lambda
				(args
					(p-ident (raw "r")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-field-access
								(receiver
									(e-ident (raw "r")))
								(segment (mode "required") (field "a"))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_tuple_access"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-tuple-access
								(e-tuple
									(e-int (raw "1"))
									(e-int (raw "2")))
								".0"))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_question"))
			(e-lambda
				(args
					(p-ident (raw "r")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-ident (raw "r"))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_tag"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-apply
								(e-tag (raw "Ok"))
								(e-int (raw "1"))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_string"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-string
								(e-string-part (raw "hi"))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_int"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-int (raw "1")))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_list"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-list
								(e-int (raw "1"))
								(e-int (raw "2"))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_record"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-record
								(field (field "a")
									(e-int (raw "1")))))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_dbg"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-dbg
							(e-ident (raw "x")))
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "after_ellipsis"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-ellipsis)
						(e-ellipsis)))))))
~~~
# FORMATTED
~~~roc
after_ident = |args| {
	_ = args
	...
}

after_call = |f| {
	_ = f(1)
	...
}

after_field = |r| {
	_ = r.a
	...
}

after_tuple_access = |_| {
	_ = (1, 2).0
	...
}

after_question = |r| {
	_ = r?
	...
}

after_tag = |_| {
	_ = Ok(1)
	...
}

after_string = |_| {
	_ = "hi"
	...
}

after_int = |_| {
	_ = 1
	...
}

after_list = |_| {
	_ = [1, 2]
	...
}

after_record = |_| {
	_ = { a: 1 }
	...
}

after_dbg = |x| {
	dbg x
	...
}

after_ellipsis = |_| {
	...
	...
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "after_ident"))
		(e-lambda
			(args
				(p-assign (ident "args")))
			(e-block
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "args"))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_call"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-block
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 351)
						(e-lookup-local
							(p-assign (ident "f")))
						(e-num (value "1"))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_field"))
		(e-lambda
			(args
				(p-assign (ident "r")))
			(e-block
				(s-let
					(p-underscore)
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "r"))))
						(segments
							(segment (name "a") (mode "required")))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_tuple_access"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-tuple-access (index "0")
						(e-tuple
							(elems
								(e-num (value "1"))
								(e-num (value "2"))))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_question"))
		(e-lambda
			(args
				(p-assign (ident "r")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "r"))))
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
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_tag"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-tag (name "Ok")
						(args
							(e-num (value "1")))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_string"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-string
						(e-literal (string "hi"))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_int"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-num (value "1")))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_list"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2")))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_record"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-record
						(fields
							(field (name "a")
								(e-num (value "1"))))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_dbg"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-dbg
					(e-lookup-local
						(p-assign (ident "x"))))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "after_ellipsis"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-expr
					(e-not-implemented))
				(e-not-implemented)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> _ret"))
		(patt (type "(b -> _ret) -> _ret2 where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "{ a: _field, .. } -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "Try(ok, err) -> Try(ok, err)"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret")))
	(expressions
		(expr (type "_arg -> _ret"))
		(expr (type "(b -> _ret) -> _ret2 where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "{ a: _field, .. } -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "Try(ok, err) -> Try(ok, err)"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))))
~~~
