# META
~~~ini
description=Add a variable with spaces
type=snippet
~~~
# SOURCE
~~~roc
add2 = x +      2
~~~
# EXPECTED
UNDEFINED VARIABLE - add_var_with_spaces.md:1:8:1:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**add_var_with_spaces.md:1:8:1:9:**
```roc
add2 = x +      2
```
       ^


# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "add2"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add2"))
		(e-dispatch-call (method "plus") (constraint-fn-var 45)
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(args
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
