# META
~~~ini
description=Variable scoping with var keyword
type=file
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
MISSING MAIN! FUNCTION - can_var_scoping_var_idents.md:2:1:8:2
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**can_var_scoping_var_idents.md:2:1:8:2:**
```roc
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
```


# TOKENS
~~~zig
LowerIdent(2:1-2:9),OpAssign(2:10-2:11),OpBar(2:12-2:13),LowerIdent(2:13-2:18),OpBar(2:18-2:19),OpenCurly(2:20-2:21),
LowerIdent(3:2-3:5),OpAssign(3:6-3:7),LowerIdent(3:8-3:13),
KwVar(4:2-4:5),LowerIdent(4:6-4:10),OpAssign(4:11-4:12),LowerIdent(4:13-4:18),OpStar(4:19-4:20),Int(4:21-4:22),
LowerIdent(6:2-6:6),OpAssign(6:7-6:8),LowerIdent(6:9-6:13),OpPlus(6:14-6:15),LowerIdent(6:16-6:19),
LowerIdent(7:2-7:5),OpPlus(7:6-7:7),LowerIdent(7:8-7:12),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @2.1-8.2
	(type-module @2.1-2.9)
	(statements
		(s-decl @2.1-8.2
			(p-ident @2.1-2.9 (raw "testFunc"))
			(e-lambda @2.12-8.2
				(args
					(p-ident @2.13-2.18 (raw "input")))
				(e-block @2.20-8.2
					(statements
						(s-decl @3.2-3.13
							(p-ident @3.2-3.5 (raw "sum"))
							(e-ident @3.8-3.13 (raw "input")))
						(s-var @4.2-4.22 (name "sum_")
							(e-binop @4.13-4.22 (op "*")
								(e-ident @4.13-4.18 (raw "input"))
								(e-int @4.21-4.22 (raw "2"))))
						(s-decl @6.2-6.19
							(p-ident @6.2-6.6 (raw "sum_"))
							(e-binop @6.9-6.19 (op "+")
								(e-ident @6.9-6.13 (raw "sum_"))
								(e-ident @6.16-6.19 (raw "sum"))))
						(e-binop @7.2-7.12 (op "+")
							(e-ident @7.2-7.5 (raw "sum"))
							(e-ident @7.8-7.12 (raw "sum_")))))))))
~~~
# FORMATTED
~~~roc
# Function showing var vs regular identifier independence
# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.9 (ident "testFunc"))
		(e-lambda @2.12-8.2
			(args
				(p-assign @2.13-2.18 (ident "input")))
			(e-block @2.20-8.2
				(s-let @3.2-3.13
					(p-assign @3.2-3.5 (ident "sum"))
					(e-lookup-local @3.8-3.13
						(p-assign @2.13-2.18 (ident "input"))))
				(s-var @4.2-4.22
					(p-assign @4.2-4.22 (ident "sum_"))
					(e-binop @4.13-4.22 (op "mul")
						(e-lookup-local @4.13-4.18
							(p-assign @2.13-2.18 (ident "input")))
						(e-int @4.21-4.22 (value "2"))))
				(s-reassign @6.2-6.6
					(p-assign @4.2-4.22 (ident "sum_"))
					(e-binop @6.9-6.19 (op "add")
						(e-lookup-local @6.9-6.13
							(p-assign @4.2-4.22 (ident "sum_")))
						(e-lookup-local @6.16-6.19
							(p-assign @3.2-3.5 (ident "sum")))))
				(e-binop @7.2-7.12 (op "add")
					(e-lookup-local @7.2-7.5
						(p-assign @3.2-3.5 (ident "sum")))
					(e-lookup-local @7.8-7.12
						(p-assign @4.2-4.22 (ident "sum_"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.9 (type "Num(_size) -> Num(_size2)")))
	(expressions
		(expr @2.12-8.2 (type "Num(_size) -> Num(_size2)"))))
~~~
