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
POLYMORPHIC VALUE - add_var_with_spaces.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**add_var_with_spaces.md:1:8:1:9:**
```roc
add2 = x +      2
```
       ^


**POLYMORPHIC VALUE**
This top-level value still has an unresolved polymorphic type:
**add_var_with_spaces.md:1:1:1:5:**
```roc
add2 = x +      2
```
^^^^


Its type is:
```roc
a where [a.plus : a, Dec -> a]
```
Add an annotation or use this value in a way that fixes its concrete type.

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
		(e-dispatch-call (method "plus") (constraint-fn-var 43)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.plus : a, Dec -> a]")))
	(expressions
		(expr (type "a where [a.plus : a, Dec -> a]"))))
~~~
