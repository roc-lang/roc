# META
~~~ini
description=Add a variable with spaces
type=file:AddVarWithSpaces.roc
~~~
# SOURCE
~~~roc
AddVarWithSpaces := {}

add2 = x +      2
~~~
# EXPECTED
UNDEFINED VARIABLE - add_var_with_spaces.md:3:8:3:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**add_var_with_spaces.md:3:8:3:9:**
```roc
add2 = x +      2
```
       ^


# TOKENS
~~~zig
UpperIdent(1:1-1:17),OpColonEqual(1:18-1:20),OpenCurly(1:21-1:22),CloseCurly(1:22-1:23),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),LowerIdent(3:8-3:9),OpPlus(3:10-3:11),Int(3:17-3:18),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.18
	(type-module @1.1-1.17)
	(statements
		(s-type-decl @1.1-1.23
			(header @1.1-1.17 (name "AddVarWithSpaces")
				(args))
			(ty-record @1.21-1.23))
		(s-decl @3.1-3.18
			(p-ident @3.1-3.5 (raw "add2"))
			(e-binop @3.8-3.18 (op "+")
				(e-ident @3.8-3.9 (raw "x"))
				(e-int @3.17-3.18 (raw "2"))))))
~~~
# FORMATTED
~~~roc
AddVarWithSpaces := {}

add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "add2"))
		(e-binop @3.8-3.18 (op "add")
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-num @3.17-3.18 (value "2"))))
	(s-nominal-decl @1.1-1.23
		(ty-header @1.1-1.17 (name "AddVarWithSpaces"))
		(ty-record @1.21-1.23)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Error")))
	(type_decls
		(nominal @1.1-1.23 (type "AddVarWithSpaces")
			(ty-header @1.1-1.17 (name "AddVarWithSpaces"))))
	(expressions
		(expr @3.8-3.18 (type "Error"))))
~~~
