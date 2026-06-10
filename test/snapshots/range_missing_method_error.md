# META
~~~ini
description=Range over non-numeric type reports missing until
type=snippet
~~~
# SOURCE
~~~roc
r = "a"..<"z"
~~~
# EXPECTED
MISSING METHOD - range_missing_method_error.md:1:5:1:14
# PROBLEMS
**MISSING METHOD**
This **until** method is being called on a value whose type doesn't have that method:
**range_missing_method_error.md:1:5:1:14:**
```roc
r = "a"..<"z"
```
    ^^^^^^^^^

The value's type, which does not have a method named **until**, is:

    Str

**Hint:** For this to work, the type would need to have a method named **until** associated with it in the type's declaration.

# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpDoubleDotLessThan,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..<")
				(e-string
					(e-string-part (raw "a")))
				(e-string
					(e-string-part (raw "z")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
