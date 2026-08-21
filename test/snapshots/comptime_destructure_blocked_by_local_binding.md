# META
~~~ini
description=A blocked destructure that reads earlier block-local bindings still names the blocking statement
type=snippet
~~~
# SOURCE
~~~roc
blocked = || {
	dbg 1
	x = 1.5
	y = 2.25
	Ok(total) = add(x, y)
	total
}

clean = || {
	x = 1.5
	y = 2.25
	Ok(total) = add(x, y)
	total
}

runtime_value = |arg| {
	dbg 1
	Ok(v) = List.get(arg, 0)
	v
}

add = |a, b|
	Try.Ok(a + b)
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - comptime_destructure_blocked_by_local_binding.md:18:2:18:7
NON EXHAUSTIVE DESTRUCTURE - comptime_destructure_blocked_by_local_binding.md:5:2:5:11
# PROBLEMS
── ✗ non exhaustive destructure ─ comptime_destructure_blocked_by_local_binding.md:18:2

This destructuring pattern doesn't cover all possible cases.

Ok(v) = List.get(arg, 0)
^^^^^

The value being destructured has type:
        Try(item, [OutOfBounds, ..])

Missing patterns:
        Err _

── ✗ non exhaustive destructure ─ comptime_destructure_blocked_by_local_binding.md:5:2

This destructuring pattern doesn't cover all possible cases.

Ok(total) = add(x, y)
^^^^^^^^^

The value being destructured has type:
        Try(ok, err)
  where [
    c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]),
    ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]),
    ok.plus : ok, c -> ok,
  ]

Missing patterns:
        Err _

This earlier statement stopped compile-time validation:

dbg 1
^^^^^
The right-hand side uses only compile-time-known values, so Roc could confirm
this pattern at compile time. Roc does not evaluate expressions at compile time
after that statement. Move this destructure above it, or handle the missing
patterns with match, ?, or ??.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
KwDbg,Int,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwDbg,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "blocked"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-dbg
							(e-int (raw "1")))
						(s-decl
							(p-ident (raw "x"))
							(e-frac (raw "1.5")))
						(s-decl
							(p-ident (raw "y"))
							(e-frac (raw "2.25")))
						(s-decl
							(p-tag (raw "Ok")
								(p-ident (raw "total")))
							(e-apply
								(e-ident (raw "add"))
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(e-ident (raw "total"))))))
		(s-decl
			(p-ident (raw "clean"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-ident (raw "x"))
							(e-frac (raw "1.5")))
						(s-decl
							(p-ident (raw "y"))
							(e-frac (raw "2.25")))
						(s-decl
							(p-tag (raw "Ok")
								(p-ident (raw "total")))
							(e-apply
								(e-ident (raw "add"))
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(e-ident (raw "total"))))))
		(s-decl
			(p-ident (raw "runtime_value"))
			(e-lambda
				(args
					(p-ident (raw "arg")))
				(e-block
					(statements
						(s-dbg
							(e-int (raw "1")))
						(s-decl
							(p-tag (raw "Ok")
								(p-ident (raw "v")))
							(e-apply
								(e-ident (raw "List.get"))
								(e-ident (raw "arg"))
								(e-int (raw "0"))))
						(e-ident (raw "v"))))))
		(s-decl
			(p-ident (raw "add"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-apply
					(e-tag (raw "Try.Ok"))
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-ident (raw "b"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "blocked"))
		(e-lambda
			(args)
			(e-block
				(s-dbg
					(e-num (value "1")))
				(s-let
					(p-assign (ident "x"))
					(e-dec-small (numerator "15") (denominator-power-of-ten "1") (value "1.5")))
				(s-let
					(p-assign (ident "y"))
					(e-dec-small (numerator "225") (denominator-power-of-ten "2") (value "2.25")))
				(s-let
					(p-applied-tag)
					(e-call (constraint-fn-var 343)
						(e-lookup-local
							(p-assign (ident "add")))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))
				(e-lookup-local
					(p-assign (ident "total"))))))
	(d-let
		(p-assign (ident "clean"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-assign (ident "x"))
					(e-dec-small (numerator "15") (denominator-power-of-ten "1") (value "1.5")))
				(s-let
					(p-assign (ident "y"))
					(e-dec-small (numerator "225") (denominator-power-of-ten "2") (value "2.25")))
				(s-let
					(p-applied-tag)
					(e-call (constraint-fn-var 371)
						(e-lookup-local
							(p-assign (ident "add")))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))
				(e-lookup-local
					(p-assign (ident "total"))))))
	(d-let
		(p-assign (ident "runtime_value"))
		(e-lambda
			(args
				(p-assign (ident "arg")))
			(e-block
				(s-dbg
					(e-num (value "1")))
				(s-let
					(p-applied-tag)
					(e-call (constraint-fn-var 294)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "arg")))
						(e-num (value "0"))))
				(e-lookup-local
					(p-assign (ident "v"))))))
	(d-let
		(p-assign (ident "add"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-nominal-external
				(builtin)
				(e-tag (name "Ok")
					(args
						(e-dispatch-call (method "plus") (constraint-fn-var 301)
							(receiver
								(e-lookup-local
									(p-assign (ident "a"))))
							(args
								(e-lookup-local
									(p-assign (ident "b")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> ok where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.plus : ok, c -> ok]"))
		(patt (type "({}) -> ok where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.plus : ok, c -> ok]"))
		(patt (type "List(item) -> item"))
		(patt (type "ok, c -> Try(ok, err) where [ok.plus : ok, c -> ok]")))
	(expressions
		(expr (type "({}) -> ok where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.plus : ok, c -> ok]"))
		(expr (type "({}) -> ok where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.plus : ok, c -> ok]"))
		(expr (type "List(item) -> item"))
		(expr (type "ok, c -> Try(ok, err) where [ok.plus : ok, c -> ok]"))))
~~~
