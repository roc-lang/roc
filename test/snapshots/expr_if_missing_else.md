# META
~~~ini
description=
type=snippet
~~~
# SOURCE
~~~roc
foo = if tru 0
~~~
# EXPECTED
IF WITHOUT ELSE - expr_if_missing_else.md:1:7:1:9
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
LowerIdent,OpAssign,KwIf,LowerIdent,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-malformed (reason "no_else")))))
~~~
# FORMATTED
~~~roc
foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
