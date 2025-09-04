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
# EXPECTED
DUPLICATE DEFINITION - can_var_scoping_var_redeclaration.md:6:2:6:13
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x_` is being redeclared in this scope.

The redeclaration is here:
**can_var_scoping_var_redeclaration.md:6:2:6:13:**
```roc
	var x_ = 10 # Redeclare var - should warn but proceed
```
	^^^^^^^^^^^

But `x_` was already defined here:
**can_var_scoping_var_redeclaration.md:5:2:5:12:**
```roc
	var x_ = 5
```
	^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpBar(4:17-4:18),Underscore(4:18-4:19),OpBar(4:19-4:20),OpenCurly(4:21-4:22),
KwVar(5:2-5:5),LowerIdent(5:6-5:8),OpAssign(5:9-5:10),Int(5:11-5:12),
KwVar(6:2-6:5),LowerIdent(6:6-6:8),OpAssign(6:9-6:10),Int(6:11-6:13),
LowerIdent(7:2-7:4),OpAssign(7:5-7:6),Int(7:7-7:9),
LowerIdent(8:2-8:4),
CloseCurly(9:1-9:2),
LowerIdent(11:1-11:7),OpAssign(11:8-11:9),LowerIdent(11:10-11:23),NoSpaceOpenRound(11:23-11:24),OpenCurly(11:24-11:25),CloseCurly(11:25-11:26),CloseRound(11:26-11:27),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.27
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @4.1-9.2
			(p-ident @4.1-4.14 (raw "redeclareTest"))
			(e-lambda @4.17-9.2
				(args
					(p-underscore))
				(e-block @4.21-9.2
					(statements
						(s-var @5.2-5.12 (name "x_")
							(e-int @5.11-5.12 (raw "5")))
						(s-var @6.2-6.13 (name "x_")
							(e-int @6.11-6.13 (raw "10")))
						(s-decl @7.2-7.9
							(p-ident @7.2-7.4 (raw "x_"))
							(e-int @7.7-7.9 (raw "15")))
						(e-ident @8.2-8.4 (raw "x_"))))))
		(s-decl @11.1-11.27
			(p-ident @11.1-11.7 (raw "result"))
			(e-apply @11.10-11.27
				(e-ident @11.10-11.23 (raw "redeclareTest"))
				(e-record @11.24-11.26)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.14 (ident "redeclareTest"))
		(e-lambda @4.17-9.2
			(args
				(p-underscore @4.18-4.19))
			(e-block @4.21-9.2
				(s-var @5.2-5.12
					(p-assign @5.2-5.12 (ident "x_"))
					(e-int @5.11-5.12 (value "5")))
				(s-var @6.2-6.13
					(p-assign @6.2-6.13 (ident "x_"))
					(e-int @6.11-6.13 (value "10")))
				(s-reassign @7.2-7.4
					(p-assign @6.2-6.13 (ident "x_"))
					(e-int @7.7-7.9 (value "15")))
				(e-lookup-local @8.2-8.4
					(p-assign @6.2-6.13 (ident "x_"))))))
	(d-let
		(p-assign @11.1-11.7 (ident "result"))
		(e-call @11.10-11.27
			(e-lookup-local @11.10-11.23
				(p-assign @4.1-4.14 (ident "redeclareTest")))
			(e-empty_record @11.24-11.26))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.14 (type "_arg -> Num(_size)"))
		(patt @11.1-11.7 (type "Num(_size)")))
	(expressions
		(expr @4.17-9.2 (type "_arg -> Num(_size)"))
		(expr @11.10-11.27 (type "Num(_size)"))))
~~~
