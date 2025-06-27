# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x_` is being redeclared in this scope.

The redeclaration is here:
**can_var_scoping_var_redeclaration.md:6:2:7:4:**
```roc
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
```

But `x_` was already defined here:
**can_var_scoping_var_redeclaration.md:5:2:6:5:**
```roc
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
```


**UNUSED VARIABLE**
Variable ``x_`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x_` to suppress this warning.
The unused variable is declared here:
**can_var_scoping_var_redeclaration.md:6:2:7:4:**
```roc
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:60),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpBar(4:17-4:18),Underscore(4:18-4:19),OpBar(4:19-4:20),OpenCurly(4:21-4:22),Newline(1:1-1:1),
KwVar(5:2-5:5),LowerIdent(5:6-5:8),OpAssign(5:9-5:10),Int(5:11-5:12),Newline(1:1-1:1),
KwVar(6:2-6:5),LowerIdent(6:6-6:8),OpAssign(6:9-6:10),Int(6:11-6:13),Newline(6:15-6:55),
LowerIdent(7:2-7:4),OpAssign(7:5-7:6),Int(7:7-7:9),Newline(7:11-7:50),
LowerIdent(8:2-8:4),Newline(1:1-1:1),
CloseCurly(9:1-9:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(11:1-11:7),OpAssign(11:8-11:9),LowerIdent(11:10-11:23),NoSpaceOpenRound(11:23-11:24),OpenCurly(11:24-11:25),CloseCurly(11:25-11:26),CloseRound(11:26-11:27),EndOfFile(11:27-11:27),
~~~
# PARSE
~~~clojure
(file @1-1-11-27
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-decl @4-1-9-2
			(p-ident @4-1-4-14 (raw "redeclareTest"))
			(e-lambda @4-17-9-2
				(args
					(p-underscore))
				(e-block @4-21-9-2
					(statements
						(s-var @5-2-6-5 (name "x_")
							(e-int @5-11-5-12 (raw "5")))
						(s-var @6-2-7-4 (name "x_")
							(e-int @6-11-6-13 (raw "10")))
						(s-decl @7-2-7-9
							(p-ident @7-2-7-4 (raw "x_"))
							(e-int @7-7-7-9 (raw "15")))
						(e-ident @8-2-8-4 (qaul "") (raw "x_"))))))
		(s-decl @11-1-11-27
			(p-ident @11-1-11-7 (raw "result"))
			(e-apply @11-10-11-27
				(e-ident @11-10-11-23 (qaul "") (raw "redeclareTest"))
				(e-record @11-24-11-26)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 89)
		(p-assign @4-1-4-14 (ident "redeclareTest") (id 73))
		(e-lambda @4-17-9-2 (id 88)
			(args
				(p-underscore @4-18-4-19 (id 74)))
			(e-block @4-21-9-2
				(s-var @5-2-6-5
					(p-assign @5-2-6-5 (ident "x_") (id 76))
					(e-int @5-11-5-12 (value "5") (id 75)))
				(s-var @6-2-7-4
					(p-assign @6-2-7-4 (ident "x_") (id 79))
					(e-int @6-11-6-13 (value "10") (id 78)))
				(s-reassign @7-2-7-4
					(p-assign @5-2-6-5 (ident "x_") (id 76))
					(e-int @7-7-7-9 (value "15") (id 82)))
				(e-lookup-local @8-2-8-4
					(pattern (id 76))))))
	(d-let (id 95)
		(p-assign @11-1-11-7 (ident "result") (id 90))
		(e-call @11-10-11-27 (id 94)
			(e-lookup-local @11-10-11-23
				(pattern (id 73)))
			(e-empty_record @11-24-11-26))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "redeclareTest") (def_var 89) (type "* ? *"))
		(d_assign (name "result") (def_var 95) (type "*")))
	(expressions
		(expr @4-17-9-2 (type "* ? *"))
		(expr @11-10-11-27 (type "*"))))
~~~
