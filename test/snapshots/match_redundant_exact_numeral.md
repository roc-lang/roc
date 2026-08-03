# META
~~~ini
description=Duplicate exact-path numeral literal match arms are redundant
type=snippet
~~~
# SOURCE
~~~roc
f : F64 -> I64
f = |x| match x {
    1e-40 => 1
    1e-40 => 2
    _ => 0
}
~~~
# EXPECTED
REDUNDANT PATTERN - match_redundant_exact_numeral.md:2:9:6:2
# PROBLEMS

┌───────────────────┐
│ REDUNDANT PATTERN ├─ The second branch of this `match` is redundant. ───────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  f = |x| match x {                                                         │
 │      1e-40 => 1                                                            │
 │      1e-40 => 2                                                            │
 │      _ => 0                                                                │
 │  }                                                                         │
 │                                                                            │
 └────────────────────────────────────── match_redundant_exact_numeral.md:2:9 ┘

    This pattern can never match because earlier patterns already cover all the
    values it would match.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Float,OpFatArrow,Int,
Float,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "F64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-match
					(e-ident (raw "x"))
					(branches
						(branch
							(p-frac (raw "1e-40"))
							(e-int (raw "1")))
						(branch
							(p-frac (raw "1e-40"))
							(e-int (raw "2")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))))
~~~
# FORMATTED
~~~roc
f : F64 -> I64
f = |x| match x {
	1e-40 => 1
	1e-40 => 2
	_ => 0
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "x"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num-from-numeral)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-num-from-numeral)))
							(value
								(e-num (value "2"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "F64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "F64 -> I64")))
	(expressions
		(expr (type "F64 -> I64"))))
~~~
