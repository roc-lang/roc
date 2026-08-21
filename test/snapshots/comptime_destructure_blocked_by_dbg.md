# META
~~~ini
description=A dbg statement blocks compile-time validation of a later refutable destructure
type=snippet
~~~
# SOURCE
~~~roc
blocked = || {
	dbg 1
	Ok(v) = List.get([1], 0)
	v
}

clean = || {
	Ok(v) = List.get([1], 0)
	v
}
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - comptime_destructure_blocked_by_dbg.md:3:2:3:7
# PROBLEMS
── ✗ non exhaustive destructure ───── comptime_destructure_blocked_by_dbg.md:3:2

This destructuring pattern doesn't cover all possible cases.

Ok(v) = List.get([1], 0)
^^^^^

The value being destructured has type:
        Try(item, [OutOfBounds, ..])
  where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)])]

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
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,Comma,Int,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,CloseSquare,Comma,Int,CloseRound,
LowerIdent,
CloseCurly,
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
							(p-tag (raw "Ok")
								(p-ident (raw "v")))
							(e-apply
								(e-ident (raw "List.get"))
								(e-list
									(e-int (raw "1")))
								(e-int (raw "0"))))
						(e-ident (raw "v"))))))
		(s-decl
			(p-ident (raw "clean"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-tag (raw "Ok")
								(p-ident (raw "v")))
							(e-apply
								(e-ident (raw "List.get"))
								(e-list
									(e-int (raw "1")))
								(e-int (raw "0"))))
						(e-ident (raw "v"))))))))
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
					(p-applied-tag)
					(e-call (constraint-fn-var 265)
						(e-lookup-external
							(builtin))
						(e-list
							(elems
								(e-num (value "1"))))
						(e-num (value "0"))))
				(e-lookup-local
					(p-assign (ident "v"))))))
	(d-let
		(p-assign (ident "clean"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-applied-tag)
					(e-call (constraint-fn-var 294)
						(e-lookup-external
							(builtin))
						(e-list
							(elems
								(e-num (value "1"))))
						(e-num (value "0"))))
				(e-lookup-local
					(p-assign (ident "v")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> item where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)])]"))
		(patt (type "({}) -> item where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "({}) -> item where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)])]"))
		(expr (type "({}) -> item where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)])]"))))
~~~
