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
UNDEFINED VARIABLE - expr_if_missing_else.md:1:10:1:13
MISSING METHOD - expr_if_missing_else.md:1:14:1:15
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `tru` in this scope.
Is there an `import` or `exposing` missing up-top?

**expr_if_missing_else.md:1:10:1:13:**
```roc
foo = if tru 0
```
         ^^^


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**expr_if_missing_else.md:1:14:1:15:**
```roc
foo = if tru 0
```
             ^

The value's type, which does not have a method named **from_numeral**, is:

    {}

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
			(e-if-without-else
				(e-ident (raw "tru"))
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-if
			(if-branches
				(if-branch
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-num (value "0"))))
			(if-else
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~
