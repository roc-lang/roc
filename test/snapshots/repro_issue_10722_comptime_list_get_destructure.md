# META
~~~ini
description=Compile-time-known List.get and List.first results satisfy refutable destructures inside functions
type=snippet
~~~
# SOURCE
~~~roc
get_at_top_level = {
	Ok(_) = List.get([1], 0)
	Ok({})
}

first_at_top_level = {
	Ok(_) = List.first([1])
	Ok({})
}

get_in_function = || {
	Ok(_) = List.get([1], 0)
	Ok({})
}

first_in_function = || {
	Ok(_) = List.first([1])
	Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,Comma,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,CloseRound,
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
			(p-ident (raw "get_at_top_level"))
			(e-block
				(statements
					(s-decl
						(p-tag (raw "Ok")
							(p-underscore))
						(e-apply
							(e-ident (raw "List.get"))
							(e-list
								(e-int (raw "1")))
							(e-int (raw "0"))))
					(e-apply
						(e-tag (raw "Ok"))
						(e-record)))))
		(s-decl
			(p-ident (raw "first_at_top_level"))
			(e-block
				(statements
					(s-decl
						(p-tag (raw "Ok")
							(p-underscore))
						(e-apply
							(e-ident (raw "List.first"))
							(e-list
								(e-int (raw "1")))))
					(e-apply
						(e-tag (raw "Ok"))
						(e-record)))))
		(s-decl
			(p-ident (raw "get_in_function"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-tag (raw "Ok")
								(p-underscore))
							(e-apply
								(e-ident (raw "List.get"))
								(e-list
									(e-int (raw "1")))
								(e-int (raw "0"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "first_in_function"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-tag (raw "Ok")
								(p-underscore))
							(e-apply
								(e-ident (raw "List.first"))
								(e-list
									(e-int (raw "1")))))
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
		(p-assign (ident "get_at_top_level"))
		(e-block
			(s-let
				(p-applied-tag)
				(e-call (constraint-fn-var 281)
					(e-lookup-external
						(builtin))
					(e-list
						(elems
							(e-num (value "1"))))
					(e-num (value "0"))))
			(e-tag (name "Ok")
				(args
					(e-empty_record)))))
	(d-let
		(p-assign (ident "first_at_top_level"))
		(e-block
			(s-let
				(p-applied-tag)
				(e-call (constraint-fn-var 308)
					(e-lookup-external
						(builtin))
					(e-list
						(elems
							(e-num (value "1"))))))
			(e-tag (name "Ok")
				(args
					(e-empty_record)))))
	(d-let
		(p-assign (ident "get_in_function"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-applied-tag)
					(e-call (constraint-fn-var 337)
						(e-lookup-external
							(builtin))
						(e-list
							(elems
								(e-num (value "1"))))
						(e-num (value "0"))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "first_in_function"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-applied-tag)
					(e-call (constraint-fn-var 359)
						(e-lookup-external
							(builtin))
						(e-list
							(elems
								(e-num (value "1"))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Ok({}), ..]"))
		(patt (type "[Ok({}), ..]"))
		(patt (type "({}) -> [Ok({}), ..]"))
		(patt (type "({}) -> [Ok({}), ..]")))
	(expressions
		(expr (type "[Ok({}), ..]"))
		(expr (type "[Ok({}), ..]"))
		(expr (type "({}) -> [Ok({}), ..]"))
		(expr (type "({}) -> [Ok({}), ..]"))))
~~~
