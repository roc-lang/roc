# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
~~~
# EXPECTED
VAR NAME MISSING `$` - can_var_scoping_var_idents.md:4:6:4:10
# PROBLEMS
── ● var name missing `$` ──────────────────── can_var_scoping_var_idents.md:4:6

The mutable binding sum_ is declared with var but its name does not start with
$.

var sum_ = input * 2 # Var with underscore - should not conflict
    ^^^^

Rename this binding and all of its uses to $sum_. The name is only a
convention; mutability comes from the var declaration.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
KwVar,LowerIdent,OpAssign,LowerIdent,OpStar,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "testFunc"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "sum"))
							(e-ident (raw "input")))
						(s-var (name "sum_")
							(e-binop (op "*")
								(e-ident (raw "input"))
								(e-int (raw "2"))))
						(s-decl
							(p-ident (raw "sum_"))
							(e-binop (op "+")
								(e-ident (raw "sum_"))
								(e-ident (raw "sum"))))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-ident (raw "sum_")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "testFunc"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-block
				(s-let
					(p-assign (ident "sum"))
					(e-lookup-local
						(p-assign (ident "input"))))
				(s-var
					(p-var-assign (ident "sum_"))
					(e-dispatch-call (method "times") (constraint-fn-var 228)
						(receiver
							(e-lookup-local
								(p-assign (ident "input"))))
						(args
							(e-num (value "2")))))
				(s-reassign
					(p-var-assign (ident "sum_"))
					(e-dispatch-call (method "plus") (constraint-fn-var 230)
						(receiver
							(e-lookup-local
								(p-var-assign (ident "sum_"))))
						(args
							(e-lookup-local
								(p-assign (ident "sum"))))))
				(e-dispatch-call (method "plus") (constraint-fn-var 232)
					(receiver
						(e-lookup-local
							(p-assign (ident "sum"))))
					(args
						(e-lookup-local
							(p-var-assign (ident "sum_")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.plus : a, a -> a, a.times : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a -> a where [a.plus : a, a -> a, a.times : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
