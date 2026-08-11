# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
f = || {
    crash 1
}
~~~
# EXPECTED
TYPE MISMATCH - fuzz_crash_067.md:2:11:2:12
# PROBLEMS
── ✗ type mismatch ────────────────────────────────────── fuzz_crash_067.md:2:11

This number is being used where a non-number type is needed.

crash 1
      ^

Other code expects this to have the type:

    Str

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
KwCrash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-crash
							(e-int (raw "1")))))))))
~~~
# FORMATTED
~~~roc
f = || {
	crash 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args)
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-runtime-error (tag "erroneous_value_expr"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret")))
	(expressions
		(expr (type "({}) -> _ret"))))
~~~
