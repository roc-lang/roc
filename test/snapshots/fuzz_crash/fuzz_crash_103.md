# META
~~~ini
description=Canonicalize panic in static_dispatch_registry invariant
type=file
~~~
# SOURCE
~~~roc
topThunk=||echo!("")main!=|_|{thunk=||echo!("")thunk()topThunk()
({}1E483647)}
~~~
# EXPECTED
EFFECTFUL FUNCTION NAME - fuzz_crash_103.md:1:1:1:9
EFFECTFUL FUNCTION NAME - fuzz_crash_103.md:1:31:1:36
INVALID NUMBER - fuzz_crash_103.md:2:4:2:12
# PROBLEMS
── ● effectful function name ───────────────────────────── fuzz_crash_103.md:1:1

This function performs an effect, so its name must end in `!`.

topThunk=||echo!("")main!=|_|{thunk=||echo!("")thunk()topThunk()
^^^^^^^^

Add a trailing ! to this function name.

── ● effectful function name ──────────────────────────── fuzz_crash_103.md:1:31

This function performs an effect, so its name must end in `!`.

topThunk=||echo!("")main!=|_|{thunk=||echo!("")thunk()topThunk()
                              ^^^^^

Add a trailing ! to this function name.

── ✗ invalid number ────────────────────────────────────── fuzz_crash_103.md:2:4

This number literal does not fit in the inferred type.

({}1E483647)}
   ^^^^^^^^

The inferred type is:

    a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,LowerIdent,OpAssign,OpBar,OpBar,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,LowerIdent,NoSpaceOpenRound,CloseRound,LowerIdent,NoSpaceOpenRound,CloseRound,
OpenRound,OpenCurly,CloseCurly,Float,CloseRound,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "topThunk"))
			(e-lambda
				(args)
				(e-apply
					(e-ident (raw "echo!"))
					(e-string
						(e-string-part (raw ""))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "thunk"))
							(e-lambda
								(args)
								(e-apply
									(e-ident (raw "echo!"))
									(e-string
										(e-string-part (raw ""))))))
						(e-apply
							(e-ident (raw "thunk")))
						(e-apply
							(e-ident (raw "topThunk")))
						(e-tuple
							(e-record)
							(e-frac (raw "1E483647")))))))))
~~~
# FORMATTED
~~~roc
topThunk = || echo!("")

main! = |_| {
	thunk = || echo!("")
	thunk()
	topThunk()
	({}, 1E483647)
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
		(p-assign (ident "topThunk"))
		(e-lambda
			(args)
			(e-call (constraint-fn-var 258)
				(e-lookup-local
					(p-assign (ident "echo!")))
				(e-string
					(e-literal (string ""))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "thunk"))
					(e-lambda
						(args)
						(e-call (constraint-fn-var 267)
							(e-lookup-local
								(p-assign (ident "echo!")))
							(e-string
								(e-literal (string ""))))))
				(s-expr
					(e-call (constraint-fn-var 268)
						(e-lookup-local
							(p-assign (ident "thunk")))))
				(s-expr
					(e-call (constraint-fn-var 270)
						(e-lookup-local
							(p-assign (ident "topThunk")))))
				(e-tuple
					(elems
						(e-empty_record)
						(e-runtime-error (tag "erroneous_value_expr"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "({}) => {}"))
		(patt (type "_arg => ({}, Error)")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "({}) => {}"))
		(expr (type "_arg => ({}, Error)"))))
~~~
