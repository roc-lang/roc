# META
~~~ini
description=
type=file:ExprIfMissingElse.roc
~~~
# SOURCE
~~~roc
ExprIfMissingElse := {}

foo = if tru 0
~~~
# EXPECTED
IF WITHOUT ELSE - expr_if_missing_else.md:3:7:3:9
UNRECOGNIZED SYNTAX - expr_if_missing_else.md:3:7:3:15
# PROBLEMS
**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**expr_if_missing_else.md:3:7:3:9:**
```roc
foo = if tru 0
```
      ^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**expr_if_missing_else.md:3:7:3:15:**
```roc
foo = if tru 0
```
      ^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
UpperIdent(1:1-1:18),OpColonEqual(1:19-1:21),OpenCurly(1:22-1:23),CloseCurly(1:23-1:24),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:13),Int(3:14-3:15),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(type-module @1.1-1.18)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.18 (name "ExprIfMissingElse")
				(args))
			(ty-record @1.22-1.24))
		(s-decl @3.1-3.15
			(p-ident @3.1-3.4 (raw "foo"))
			(e-malformed @3.7-3.15 (reason "no_else")))))
~~~
# FORMATTED
~~~roc
ExprIfMissingElse := {}

foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.18 (name "ExprIfMissingElse"))
		(ty-record @1.22-1.24)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error")))
	(type_decls
		(nominal @1.1-1.24 (type "ExprIfMissingElse")
			(ty-header @1.1-1.18 (name "ExprIfMissingElse"))))
	(expressions
		(expr @3.7-3.15 (type "Error"))))
~~~
