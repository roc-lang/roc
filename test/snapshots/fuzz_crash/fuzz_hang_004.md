# META
~~~ini
description=canonicalize hang: recursive slice pattern matcher with partial tokens
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - fuzz_hang_004.md:1:24:1:26
TYPE MISMATCH - fuzz_hang_004.md:1:10:1:11
INVALID TUPLE ACCESS - fuzz_hang_004.md:1:27:1:35
REDUNDANT PATTERN - fuzz_hang_004.md:1:4:1:37
NON EXHAUSTIVE MATCH - fuzz_hang_004.md:1:4:1:37
# PROBLEMS
── ✗ empty tuple not allowed ───────────────────────────── fuzz_hang_004.md:1:24

I am part way through parsing this tuple, but it is empty.

s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
                       ^^

If you want to represent nothing, try using an empty record: {}.

── ✗ type mismatch ─────────────────────────────────────── fuzz_hang_004.md:1:10

This number is being used where a non-number type is needed.

s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
         ^

The type was determined to be non-numeric here:

s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
           ^^
Other code expects this to have the type:

    List(_b)

── ✗ invalid tuple access ──────────────────────────────── fuzz_hang_004.md:1:27

This value is not a tuple, so it has no .70000 element.

s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
                          ^^^^^^^^

── ● redundant pattern ──────────────────────────────────── fuzz_hang_004.md:1:4

The second branch of this match is redundant.

s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This pattern can never match because earlier patterns already cover all the
values it would match.

── ✗ non exhaustive match ───────────────────────────────── fuzz_hang_004.md:1:4

This match expression doesn't cover all possible cases.

s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The value being matched on has type:
        List(_b)

Missing patterns:
        [_, ..]

Hint: Add branches to handle these cases, or use _ to match anything.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,KwMatch,Int,OpenCurly,OpenSquare,CloseSquare,OpFatArrow,OpenSquare,CloseSquare,OpenSquare,CloseSquare,OpFatArrow,OpenCurly,OpenCurly,NoSpaceOpenRound,CloseRound,CloseCurly,OpenCurly,CloseCurly,NoSpaceDotInt,CloseCurly,CloseCurly,CloseCurly,LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "s"))
			(e-block
				(statements
					(e-match
						(e-int (raw "0"))
						(branches
							(branch
								(p-list)
								(e-list))
							(branch
								(p-list)
								(e-block
									(statements
										(e-block
											(statements
												(e-tuple)))
										(e-tuple-access
											(e-record)
											".70000")))))))))
		(s-decl
			(p-ident (raw "a"))
			(e-lambda
				(args)
				(e-record)))))
~~~
# FORMATTED
~~~roc
s = {
	match 0 {
		[] => []
		[] => {
			{
				()
			}
			{}.70000
		}
	}
}

a = || {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "s"))
		(e-block
			(e-match
				(match
					(cond
						(e-runtime-error (tag "erroneous_value_expr")))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns))))
							(value
								(e-empty_list)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns))))
							(value
								(e-runtime-error (tag "erroneous_value_expr")))))))))
	(d-let
		(p-assign (ident "a"))
		(e-lambda
			(args)
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(_b)"))
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "List(_b)"))
		(expr (type "({}) -> {}"))))
~~~
