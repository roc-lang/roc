# META
~~~ini
description=Calling one generic associated method from another and binding the result to an annotated local must not produce a spurious type mismatch whose displayed current and required types are identical (issue 9670)
type=file:Parser.roc
~~~
# SOURCE
~~~roc
Parser(input, a) :: {runParser : input -> a}.{
    build : (input -> a) -> Parser(input, a)
    build = |fun| {runParser : fun}

    run : Parser(input, a), input -> a
    run = |{runParser}, input| runParser(input)

    const : a -> Parser(input, a)
    const = |val| build(|_i| val)

    apply : Parser(input, (a -> b)), Parser(input, a) -> Parser(input, b)
    apply = |fp, vp| build(|i| Parser.run(fp, i)(Parser.run(vp, i)))

    many : Parser(input, a) -> Parser(input, List(a))
    many = |p| build(|i| [Parser.run(p, i)])

    sep_by1 : Parser(input, a), Parser(input, sep) -> Parser(input, List(a))
    sep_by1 = |parser, separator| {
        sep_then_parser = const(|_| |val| val)
          .apply(separator)
          .apply(parser)

        const(|val| |vals| List.prepend(vals, val))
          .apply(parser)
          .apply(many(sep_then_parser))
    }

    sep_by : Parser(input, a), Parser(input, sep) -> Parser(input, List(a))
    sep_by = |parser, separator| {
        sb1 : Parser(input, List(a))
        sb1 = sep_by1(parser, separator)
        sb1
    }
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**repro_issue_9670_equivalent_type_mismatch.md:31:15:31:41:**
```roc
        sb1 = sep_by1(parser, separator)
```
              ^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:

    Parser(input, List(a))

But the annotation says it should be:

    Parser(input, List(a))

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpDoubleColon,OpenCurly,LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,CloseCurly,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpBar,NamedUnderscore,OpBar,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,OpenSquare,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
DotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
DotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
DotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
DotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Parser")
				(args
					(ty-var (raw "input"))
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "runParser")
					(ty-fn
						(ty-var (raw "input"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "build")
					(ty-fn
						(ty-fn
							(ty-var (raw "input"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))))
				(s-decl
					(p-ident (raw "build"))
					(e-lambda
						(args
							(p-ident (raw "fun")))
						(e-record
							(field (field "runParser")
								(e-ident (raw "fun"))))))
				(s-type-anno (name "run")
					(ty-fn
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))
						(ty-var (raw "input"))
						(ty-var (raw "a"))))
				(s-decl
					(p-ident (raw "run"))
					(e-lambda
						(args
							(p-record
								(field (name "runParser") (rest false)))
							(p-ident (raw "input")))
						(e-apply
							(e-ident (raw "runParser"))
							(e-ident (raw "input")))))
				(s-type-anno (name "const")
					(ty-fn
						(ty-var (raw "a"))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))))
				(s-decl
					(p-ident (raw "const"))
					(e-lambda
						(args
							(p-ident (raw "val")))
						(e-apply
							(e-ident (raw "build"))
							(e-lambda
								(args
									(p-ident (raw "_i")))
								(e-ident (raw "val"))))))
				(s-type-anno (name "apply")
					(ty-fn
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-fn
								(ty-var (raw "a"))
								(ty-var (raw "b"))))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "b")))))
				(s-decl
					(p-ident (raw "apply"))
					(e-lambda
						(args
							(p-ident (raw "fp"))
							(p-ident (raw "vp")))
						(e-apply
							(e-ident (raw "build"))
							(e-lambda
								(args
									(p-ident (raw "i")))
								(e-apply
									(e-apply
										(e-ident (raw "Parser.run"))
										(e-ident (raw "fp"))
										(e-ident (raw "i")))
									(e-apply
										(e-ident (raw "Parser.run"))
										(e-ident (raw "vp"))
										(e-ident (raw "i"))))))))
				(s-type-anno (name "many")
					(ty-fn
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "a"))))))
				(s-decl
					(p-ident (raw "many"))
					(e-lambda
						(args
							(p-ident (raw "p")))
						(e-apply
							(e-ident (raw "build"))
							(e-lambda
								(args
									(p-ident (raw "i")))
								(e-list
									(e-apply
										(e-ident (raw "Parser.run"))
										(e-ident (raw "p"))
										(e-ident (raw "i"))))))))
				(s-type-anno (name "sep_by1")
					(ty-fn
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "sep")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "a"))))))
				(s-decl
					(p-ident (raw "sep_by1"))
					(e-lambda
						(args
							(p-ident (raw "parser"))
							(p-ident (raw "separator")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "sep_then_parser"))
									(e-method-call (method ".apply")
										(receiver
											(e-method-call (method ".apply")
												(receiver
													(e-apply
														(e-ident (raw "const"))
														(e-lambda
															(args
																(p-underscore))
															(e-lambda
																(args
																	(p-ident (raw "val")))
																(e-ident (raw "val"))))))
												(args
													(e-ident (raw "separator")))))
										(args
											(e-ident (raw "parser")))))
								(e-method-call (method ".apply")
									(receiver
										(e-method-call (method ".apply")
											(receiver
												(e-apply
													(e-ident (raw "const"))
													(e-lambda
														(args
															(p-ident (raw "val")))
														(e-lambda
															(args
																(p-ident (raw "vals")))
															(e-apply
																(e-ident (raw "List.prepend"))
																(e-ident (raw "vals"))
																(e-ident (raw "val")))))))
											(args
												(e-ident (raw "parser")))))
									(args
										(e-apply
											(e-ident (raw "many"))
											(e-ident (raw "sep_then_parser")))))))))
				(s-type-anno (name "sep_by")
					(ty-fn
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-var (raw "sep")))
						(ty-apply
							(ty (name "Parser"))
							(ty-var (raw "input"))
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "a"))))))
				(s-decl
					(p-ident (raw "sep_by"))
					(e-lambda
						(args
							(p-ident (raw "parser"))
							(p-ident (raw "separator")))
						(e-block
							(statements
								(s-type-anno (name "sb1")
									(ty-apply
										(ty (name "Parser"))
										(ty-var (raw "input"))
										(ty-apply
											(ty (name "List"))
											(ty-var (raw "a")))))
								(s-decl
									(p-ident (raw "sb1"))
									(e-apply
										(e-ident (raw "sep_by1"))
										(e-ident (raw "parser"))
										(e-ident (raw "separator"))))
								(e-ident (raw "sb1"))))))))))
~~~
# FORMATTED
~~~roc
Parser(input, a) :: { runParser : input -> a }.{
	build : (input -> a) -> Parser(input, a)
	build = |fun| { runParser: fun }

	run : Parser(input, a), input -> a
	run = |{ runParser }, input| runParser(input)

	const : a -> Parser(input, a)
	const = |val| build(|_i| val)

	apply : Parser(input, (a -> b)), Parser(input, a) -> Parser(input, b)
	apply = |fp, vp| build(|i| Parser.run(fp, i)(Parser.run(vp, i)))

	many : Parser(input, a) -> Parser(input, List(a))
	many = |p| build(|i| [Parser.run(p, i)])

	sep_by1 : Parser(input, a), Parser(input, sep) -> Parser(input, List(a))
	sep_by1 = |parser, separator| {
		sep_then_parser = const(|_| |val| val)
			.apply(separator)
			.apply(parser)

		const(|val| |vals| List.prepend(vals, val))
			.apply(parser)
			.apply(many(sep_then_parser))
	}

	sep_by : Parser(input, a), Parser(input, sep) -> Parser(input, List(a))
	sep_by = |parser, separator| {
		sb1 : Parser(input, List(a))
		sb1 = sep_by1(parser, separator)
		sb1
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Parser.build"))
		(e-lambda
			(args
				(p-assign (ident "fun")))
			(e-record
				(fields
					(field (name "runParser")
						(e-lookup-local
							(p-assign (ident "fun")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "input"))
						(ty-rigid-var (name "a"))))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "Parser.run"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "runParser") (ident "runParser")
							(required
								(p-assign (ident "runParser"))))))
				(p-assign (ident "input")))
			(e-call (constraint-fn-var 250)
				(e-lookup-local
					(p-assign (ident "runParser")))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Parser") (local)
					(ty-rigid-var (name "input"))
					(ty-rigid-var (name "a")))
				(ty-rigid-var-lookup (ty-rigid-var (name "input")))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "Parser.const"))
		(e-lambda
			(args
				(p-assign (ident "val")))
			(e-call (constraint-fn-var 269)
				(e-lookup-local
					(p-assign (ident "Parser.build")))
				(e-closure
					(captures
						(capture (ident "val")))
					(e-lambda
						(args
							(p-assign (ident "_i")))
						(e-lookup-local
							(p-assign (ident "val")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var (name "input"))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "Parser.apply"))
		(e-lambda
			(args
				(p-assign (ident "fp"))
				(p-assign (ident "vp")))
			(e-call (constraint-fn-var 321)
				(e-lookup-local
					(p-assign (ident "Parser.build")))
				(e-closure
					(captures
						(capture (ident "fp"))
						(capture (ident "vp")))
					(e-lambda
						(args
							(p-assign (ident "i")))
						(e-call (constraint-fn-var 320)
							(e-call (constraint-fn-var 312)
								(e-lookup-local
									(p-assign (ident "Parser.run")))
								(e-lookup-local
									(p-assign (ident "fp")))
								(e-lookup-local
									(p-assign (ident "i"))))
							(e-call (constraint-fn-var 319)
								(e-lookup-local
									(p-assign (ident "Parser.run")))
								(e-lookup-local
									(p-assign (ident "vp")))
								(e-lookup-local
									(p-assign (ident "i")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Parser") (local)
					(ty-rigid-var (name "input"))
					(ty-parens
						(ty-fn (effectful false)
							(ty-rigid-var (name "a"))
							(ty-rigid-var (name "b")))))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "Parser.many"))
		(e-lambda
			(args
				(p-assign (ident "p")))
			(e-call (constraint-fn-var 362)
				(e-lookup-local
					(p-assign (ident "Parser.build")))
				(e-closure
					(captures
						(capture (ident "p")))
					(e-lambda
						(args
							(p-assign (ident "i")))
						(e-list
							(elems
								(e-call (constraint-fn-var 359)
									(e-lookup-local
										(p-assign (ident "Parser.run")))
									(e-lookup-local
										(p-assign (ident "p")))
									(e-lookup-local
										(p-assign (ident "i"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Parser") (local)
					(ty-rigid-var (name "input"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(d-let
		(p-assign (ident "Parser.sep_by1"))
		(e-lambda
			(args
				(p-assign (ident "parser"))
				(p-assign (ident "separator")))
			(e-block
				(s-let
					(p-assign (ident "sep_then_parser"))
					(e-dispatch-call (method "apply") (constraint-fn-var 420)
						(receiver
							(e-dispatch-call (method "apply") (constraint-fn-var 404)
								(receiver
									(e-call (constraint-fn-var 403)
										(e-lookup-local
											(p-assign (ident "Parser.const")))
										(e-lambda
											(args
												(p-underscore))
											(e-lambda
												(args
													(p-assign (ident "val")))
												(e-lookup-local
													(p-assign (ident "val")))))))
								(args
									(e-lookup-local
										(p-assign (ident "separator"))))))
						(args
							(e-lookup-local
								(p-assign (ident "parser"))))))
				(e-dispatch-call (method "apply") (constraint-fn-var 487)
					(receiver
						(e-dispatch-call (method "apply") (constraint-fn-var 460)
							(receiver
								(e-call (constraint-fn-var 459)
									(e-lookup-local
										(p-assign (ident "Parser.const")))
									(e-lambda
										(args
											(p-assign (ident "val")))
										(e-closure
											(captures
												(capture (ident "val")))
											(e-lambda
												(args
													(p-assign (ident "vals")))
												(e-call (constraint-fn-var 458)
													(e-lookup-external
														(builtin))
													(e-lookup-local
														(p-assign (ident "vals")))
													(e-lookup-local
														(p-assign (ident "val")))))))))
							(args
								(e-lookup-local
									(p-assign (ident "parser"))))))
					(args
						(e-call (constraint-fn-var 486)
							(e-lookup-local
								(p-assign (ident "Parser.many")))
							(e-lookup-local
								(p-assign (ident "sep_then_parser"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Parser") (local)
					(ty-rigid-var (name "input"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-rigid-var (name "sep")))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(d-let
		(p-assign (ident "Parser.sep_by"))
		(e-lambda
			(args
				(p-assign (ident "parser"))
				(p-assign (ident "separator")))
			(e-block
				(s-let
					(p-assign (ident "sb1"))
					(e-call (constraint-fn-var 566)
						(e-lookup-local
							(p-assign (ident "Parser.sep_by1")))
						(e-lookup-local
							(p-assign (ident "parser")))
						(e-lookup-local
							(p-assign (ident "separator")))))
				(e-lookup-local
					(p-assign (ident "sb1")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Parser") (local)
					(ty-rigid-var (name "input"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-rigid-var (name "sep")))
				(ty-apply (name "Parser") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(s-nominal-decl
		(ty-header (name "Parser")
			(ty-args
				(ty-rigid-var (name "input"))
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "runParser")
				(ty-fn (effectful false)
					(ty-rigid-var-lookup (ty-rigid-var (name "input")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(input -> a) -> Parser(input, a)"))
		(patt (type "Parser(input, a), input -> a"))
		(patt (type "a -> Parser(input, a)"))
		(patt (type "Parser(input, a -> b), Parser(input, a) -> Parser(input, b)"))
		(patt (type "Parser(input, a) -> Parser(input, List(a))"))
		(patt (type "Parser(input, a), Parser(input, sep) -> Parser(input, List(a))"))
		(patt (type "Parser(input, a), Parser(input, sep) -> Parser(input, List(a))")))
	(type_decls
		(nominal (type "Parser(input, a)")
			(ty-header (name "Parser")
				(ty-args
					(ty-rigid-var (name "input"))
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "(input -> a) -> Parser(input, a)"))
		(expr (type "Parser(input, a), input -> a"))
		(expr (type "a -> Parser(input, a)"))
		(expr (type "Parser(input, a -> b), Parser(input, a) -> Parser(input, b)"))
		(expr (type "Parser(input, a) -> Parser(input, List(a))"))
		(expr (type "Parser(input, a), Parser(input, sep) -> Parser(input, List(a))"))
		(expr (type "Parser(input, a), Parser(input, sep) -> Parser(input, List(a))"))))
~~~
