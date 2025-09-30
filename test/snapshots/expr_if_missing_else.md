# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
foo = if tru 0
~~~
# EXPECTED
IF WITHOUT ELSE - expr_if_missing_else.md:1:7:1:9
MISSING MAIN! FUNCTION - expr_if_missing_else.md:1:1:1:15
UNRECOGNIZED SYNTAX - expr_if_missing_else.md:1:7:1:15
# PROBLEMS
**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**expr_if_missing_else.md:1:7:1:9:**
```roc
foo = if tru 0
```
      ^^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**expr_if_missing_else.md:1:1:1:15:**
```roc
foo = if tru 0
```
^^^^^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**expr_if_missing_else.md:1:7:1:15:**
```roc
foo = if tru 0
```
      ^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),KwIf(1:7-1:9),LowerIdent(1:10-1:13),Int(1:14-1:15),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.15
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.15
			(p-ident @1.1-1.4 (raw "foo"))
			(e-malformed @1.7-1.15 (reason "no_else")))))
~~~
# FORMATTED
~~~roc
foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Error")))
	(expressions
		(expr @1.7-1.15 (type "Error"))))
~~~
