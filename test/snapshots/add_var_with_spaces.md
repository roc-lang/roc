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
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),LowerIdent(1:8-1:9),OpPlus(1:10-1:11),Int(1:17-1:18),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.18
	(type-module @1.1-1.5)
	(statements
		(s-decl @1.1-1.18
			(p-ident @1.1-1.5 (raw "add2"))
			(e-binop @1.8-1.18 (op "+")
				(e-ident @1.8-1.9 (raw "x"))
				(e-int @1.17-1.18 (raw "2"))))))
~~~
# FORMATTED
~~~roc
add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.5 (ident "add2"))
		(e-binop @1.8-1.18 (op "add")
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-num @1.17-1.18 (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.5 (type "Error")))
	(expressions
		(expr @1.8-1.18 (type "Error"))))
~~~
