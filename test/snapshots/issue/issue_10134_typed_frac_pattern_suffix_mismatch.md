# META
~~~ini
description=An explicit fractional pattern suffix constrains the pattern independently of its match context
type=snippet
~~~
# SOURCE
~~~roc
classify : F64 -> I64
classify = |n| match n {
	1.5.F32 => 1
	_ => 0
}
~~~
# EXPECTED
TYPE MISMATCH - issue_10134_typed_frac_pattern_suffix_mismatch.md:2:16:2:16
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ The first pattern in this `match` is incompatible. ────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  classify = |n| match n {                                                  │
 │      1.5.F32 => 1                                                          │
 │      _ => 0                                                                │
 │  }                                                                         │
 │                                                                            │
 └───────────────────── issue_10134_typed_frac_pattern_suffix_mismatch.md:2:2 ┘

    The first pattern is trying to match:

        F32

    But the expression between the `match` parenthesis has the type:

        F64

    These can never match! Either the pattern or expression has a problem.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Float,NoSpaceDotUpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "classify")
			(ty-fn
				(ty (name "F64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "classify"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-match
					(e-ident (raw "n"))
					(branches
						(branch
							(p-typed-frac (raw "1.5") (type "F32"))
							(e-int (raw "1")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "classify"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "n"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-small-dec)))
							(value
								(e-num (value "1"))))
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
