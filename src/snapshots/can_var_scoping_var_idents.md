# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_var_scoping_var_idents.md:11:1:11:3
UNEXPECTED TOKEN IN EXPRESSION - can_var_scoping_var_idents.md:11:2:11:4
UNEXPECTED TOKEN IN EXPRESSION - can_var_scoping_var_idents.md:11:3:11:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_var_scoping_var_idents.md:11:1:11:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_var_scoping_var_idents.md:11:2:11:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_var_scoping_var_idents.md:11:3:11:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:58),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:18),OpBar(4:18-4:19),OpenCurly(4:20-4:21),Newline(1:1-1:1),
LowerIdent(5:2-5:5),OpAssign(5:6-5:7),LowerIdent(5:8-5:13),Newline(5:15-5:34),
KwVar(6:2-6:5),LowerIdent(6:6-6:10),OpAssign(6:11-6:12),LowerIdent(6:13-6:18),OpStar(6:19-6:20),Int(6:21-6:22),Newline(6:24-6:66),
Newline(1:1-1:1),
LowerIdent(8:2-8:6),OpAssign(8:7-8:8),LowerIdent(8:9-8:13),OpPlus(8:14-8:15),LowerIdent(8:16-8:19),Newline(8:21-8:48),
LowerIdent(9:2-9:5),OpPlus(9:6-9:7),LowerIdent(9:8-9:12),Newline(9:14-9:40),
CloseCurly(10:1-10:2),Newline(1:1-1:1),
MalformedUnknownToken(11:1-11:2),MalformedUnknownToken(11:2-11:3),MalformedUnknownToken(11:3-11:4),EndOfFile(11:4-11:4),
~~~
# PARSE
~~~clojure
(file @1.1-11.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @4.1-10.2
			(p-ident @4.1-4.9 (raw "testFunc"))
			(e-lambda @4.12-10.2
				(args
					(p-ident @4.13-4.18 (raw "input")))
				(e-block @4.20-10.2
					(statements
						(s-decl @5.2-5.13
							(p-ident @5.2-5.5 (raw "sum"))
							(e-ident @5.8-5.13 (raw "input")))
						(s-var @6.2-8.6 (name "sum_")
							(e-binop @6.13-8.6 (op "*")
								(e-ident @6.13-6.18 (raw "input"))
								(e-int @6.21-6.22 (raw "2"))))
						(s-decl @8.2-9.5
							(p-ident @8.2-8.6 (raw "sum_"))
							(e-binop @8.9-9.5 (op "+")
								(e-ident @8.9-8.13 (raw "sum_"))
								(e-ident @8.16-8.19 (raw "sum"))))
						(e-binop @9.2-10.2 (op "+")
							(e-ident @9.2-9.5 (raw "sum"))
							(e-ident @9.8-9.12 (raw "sum_")))))))
		(e-malformed @11.1-11.3 (reason "expr_unexpected_token"))
		(e-malformed @11.2-11.4 (reason "expr_unexpected_token"))
		(e-malformed @11.3-11.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

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
		(p-assign @4.1-4.9 (ident "testFunc"))
		(e-lambda @4.12-10.2
			(args
				(p-assign @4.13-4.18 (ident "input")))
			(e-block @4.20-10.2
				(s-let @5.2-5.13
					(p-assign @5.2-5.5 (ident "sum"))
					(e-lookup-local @5.8-5.13
						(pattern @4.13-4.18)))
				(s-var @6.2-8.6
					(p-assign @6.2-8.6 (ident "sum_"))
					(e-binop @6.13-8.6 (op "mul")
						(e-lookup-local @6.13-6.18
							(pattern @4.13-4.18))
						(e-int @6.21-6.22 (value "2"))))
				(s-reassign @8.2-8.6
					(p-assign @6.2-8.6 (ident "sum_"))
					(e-binop @8.9-9.5 (op "add")
						(e-lookup-local @8.9-8.13
							(pattern @6.2-8.6))
						(e-lookup-local @8.16-8.19
							(pattern @5.2-5.5))))
				(e-binop @9.2-10.2 (op "add")
					(e-lookup-local @9.2-9.5
						(pattern @5.2-5.5))
					(e-lookup-local @9.8-9.12
						(pattern @6.2-8.6)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.9 (type "* -> *")))
	(expressions
		(expr @4.12-10.2 (type "* -> *"))))
~~~
