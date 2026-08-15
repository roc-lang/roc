# META
~~~ini
description=fuzz crash: formatter parser output instability
type=snippet
~~~
# SOURCE
~~~roc
i:U
d:i
o=||{D()=0}
()=()
~~~
# EXPECTED
UNDECLARED TYPE - fuzz_crash_082.md:1:3:1:4
EMPTY TUPLE NOT ALLOWED - fuzz_crash_082.md:4:4:4:6
DECLARATION HAS NO VALUE - fuzz_crash_082.md:1:1:1:4
DECLARATION HAS NO VALUE - fuzz_crash_082.md:2:1:2:4
MISSING METHOD - fuzz_crash_082.md:3:10:3:11
# PROBLEMS
── ✗ undeclared type ───────────────────────────────────── fuzz_crash_082.md:1:3

The type U is not declared in this scope.

i:U
  ^

── ✗ empty tuple not allowed ───────────────────────────── fuzz_crash_082.md:4:4

I am part way through parsing this tuple, but it is empty.

()=()
   ^^

If you want to represent nothing, try using an empty record: {}.

── ● declaration has no value ──────────────────────────── fuzz_crash_082.md:1:1

This declaration has a type annotation but no implementation.

i:U
^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ● declaration has no value ──────────────────────────── fuzz_crash_082.md:2:1

This declaration has a type annotation but no implementation.

d:i
^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ✗ missing method ───────────────────────────────────── fuzz_crash_082.md:3:10

This from_numeral method is being called on a value whose type doesn't have
that method.

o=||{D()=0}
         ^

The value's type, which does not have a method named from_numeral, is:

    [D]

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,UpperIdent,NoSpaceOpenRound,CloseRound,OpAssign,Int,CloseCurly,
OpenRound,CloseRound,OpAssign,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "i")
			(ty (name "U")))
		(s-type-anno (name "d")
			(ty-var (raw "i")))
		(s-decl
			(p-ident (raw "o"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-tag (raw "D"))
							(e-int (raw "0")))))))
		(s-decl
			(p-tuple)
			(e-tuple))))
~~~
# FORMATTED
~~~roc
i : U

d : i

o = || {
	D() = 0
}

() = ()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "i"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "d"))
		(e-anno-only)
		(annotation
			(ty-rigid-var (name "i"))))
	(d-let
		(p-assign (ident "o"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-applied-tag)
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-empty_record))))
	(d-let
		(p-tuple
			(patterns))
		(e-runtime-error (tag "empty_tuple"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "i"))
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "Error"))
		(expr (type "i"))
		(expr (type "({}) -> {}"))
		(expr (type "Error"))))
~~~
