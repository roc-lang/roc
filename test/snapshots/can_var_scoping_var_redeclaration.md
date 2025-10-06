# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# EXPECTED
DUPLICATE DEFINITION - can_var_scoping_var_redeclaration.md:4:2:4:13
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x_` is being redeclared in this scope.

The redeclaration is here:
**can_var_scoping_var_redeclaration.md:4:2:4:13:**
```roc
	var x_ = 10 # Redeclare var - should warn but proceed
```
	^^^^^^^^^^^

But `x_` was already defined here:
**can_var_scoping_var_redeclaration.md:3:2:3:12:**
```roc
	var x_ = 5
```
	^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent(2:1-2:14),OpAssign(2:15-2:16),OpBar(2:17-2:18),Underscore(2:18-2:19),OpBar(2:19-2:20),OpenCurly(2:21-2:22),
KwVar(3:2-3:5),LowerIdent(3:6-3:8),OpAssign(3:9-3:10),Int(3:11-3:12),
KwVar(4:2-4:5),LowerIdent(4:6-4:8),OpAssign(4:9-4:10),Int(4:11-4:13),
LowerIdent(5:2-5:4),OpAssign(5:5-5:6),Int(5:7-5:9),
LowerIdent(6:2-6:4),
CloseCurly(7:1-7:2),
LowerIdent(9:1-9:7),OpAssign(9:8-9:9),LowerIdent(9:10-9:23),NoSpaceOpenRound(9:23-9:24),OpenCurly(9:24-9:25),CloseCurly(9:25-9:26),CloseRound(9:26-9:27),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @2.1-9.27
	(type-module @2.1-2.14)
	(statements
		(s-decl @2.1-7.2
			(p-ident @2.1-2.14 (raw "redeclareTest"))
			(e-lambda @2.17-7.2
				(args
					(p-underscore))
				(e-block @2.21-7.2
					(statements
						(s-var @3.2-3.12 (name "x_")
							(e-int @3.11-3.12 (raw "5")))
						(s-var @4.2-4.13 (name "x_")
							(e-int @4.11-4.13 (raw "10")))
						(s-decl @5.2-5.9
							(p-ident @5.2-5.4 (raw "x_"))
							(e-int @5.7-5.9 (raw "15")))
						(e-ident @6.2-6.4 (raw "x_"))))))
		(s-decl @9.1-9.27
			(p-ident @9.1-9.7 (raw "result"))
			(e-apply @9.10-9.27
				(e-ident @9.10-9.23 (raw "redeclareTest"))
				(e-record @9.24-9.26)))))
~~~
# FORMATTED
~~~roc
# Test var redeclaration (should produce shadowing warning)
# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.14 (ident "redeclareTest"))
		(e-lambda @2.17-7.2
			(args
				(p-underscore @2.18-2.19))
			(e-block @2.21-7.2
				(s-var @3.2-3.12
					(p-assign @3.2-3.12 (ident "x_"))
					(e-num @3.11-3.12 (value "5")))
				(s-var @4.2-4.13
					(p-assign @4.2-4.13 (ident "x_"))
					(e-num @4.11-4.13 (value "10")))
				(s-reassign @5.2-5.4
					(p-assign @4.2-4.13 (ident "x_"))
					(e-num @5.7-5.9 (value "15")))
				(e-lookup-local @6.2-6.4
					(p-assign @4.2-4.13 (ident "x_"))))))
	(d-let
		(p-assign @9.1-9.7 (ident "result"))
		(e-call @9.10-9.27
			(e-lookup-local @9.10-9.23
				(p-assign @2.1-2.14 (ident "redeclareTest")))
			(e-empty_record @9.24-9.26))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.14 (type "_arg -> Num(_size)"))
		(patt @9.1-9.7 (type "Num(_size)")))
	(expressions
		(expr @2.17-7.2 (type "_arg -> Num(_size)"))
		(expr @9.10-9.27 (type "Num(_size)"))))
~~~
