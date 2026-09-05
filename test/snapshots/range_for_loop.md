# META
~~~ini
description=for loop over an inclusive range
type=snippet
~~~
# SOURCE
~~~roc
total : U64
total = {
    var sum_ = 0
    for i in 1..=5 {
        sum_ = sum_ + i
    }
    sum_
}
~~~
# EXPECTED
VAR NAME MISSING `$` - range_for_loop.md:3:9:3:13
# PROBLEMS
── ● var name missing `$` ──────────────────────────────── range_for_loop.md:3:9

The mutable binding sum_ is declared with var but its name does not start with
$.

var sum_ = 0
    ^^^^

Rename this binding and all of its uses to $sum_. The name is only a
convention; mutability comes from the var declaration.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,Int,OpDoubleDotEquals,Int,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "total")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "total"))
			(e-block
				(statements
					(s-var (name "sum_")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "i"))
						(e-binop (op "..=")
							(e-int (raw "1"))
							(e-int (raw "5")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "sum_"))
									(e-binop (op "+")
										(e-ident (raw "sum_"))
										(e-ident (raw "i")))))))
					(e-ident (raw "sum_")))))))
~~~
# FORMATTED
~~~roc
total : U64
total = {
	var sum_ = 0
	for i in 1..=5 {
		sum_ = sum_ + i
	}
	sum_
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "total"))
		(e-block
			(s-var
				(p-var-assign (ident "sum_"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "i"))
				(e-dispatch-call (method "range_inclusive_to") (constraint-fn-var 249)
					(receiver
						(e-num (value "1")))
					(args
						(e-num (value "5"))))
				(e-block
					(s-reassign
						(p-var-assign (ident "sum_"))
						(e-dispatch-call (method "plus") (constraint-fn-var 311)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "sum_"))))
							(args
								(e-lookup-local
									(p-assign (ident "i"))))))
					(e-empty_record)))
			(e-lookup-local
				(p-var-assign (ident "sum_"))))
		(annotation
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
