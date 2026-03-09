# META
~~~ini
description=Field access on scientific notation should produce an error, not crash
type=snippet
~~~
# SOURCE
~~~roc
y = 1E10.e
~~~
# EXPECTED
MISSING METHOD - scientific_notation_field_access.md:1:5:1:9
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**scientific_notation_field_access.md:1:5:1:9:**
```roc
y = 1E10.e
```
    ^^^^

The value's type, which does not have a method named **from_numeral**, is:

    { e: _field }

# TOKENS
~~~zig
LowerIdent,OpAssign,Float,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "y"))
			(e-field-access
				(e-frac (raw "1E10"))
				(e-ident (raw "e"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "y"))
		(e-dot-access (field "e")
			(receiver
				(e-frac-dec (value "10000000000"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
