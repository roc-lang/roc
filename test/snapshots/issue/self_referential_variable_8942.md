# META
~~~ini
description=Regression test for stack overflow with self-referential variable (issue #8942)
type=snippet
~~~
# SOURCE
~~~roc
a = a
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - self_referential_variable_8942.md:1:5:1:6
# PROBLEMS
**INVALID ASSIGNMENT TO ITSELF**
The value `a` is assigned to itself, which would cause an infinite loop at runtime.

Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.

**self_referential_variable_8942.md:1:5:1:6:**
```roc
a = a
```
    ^


# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-ident (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "self_referential_definition"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
