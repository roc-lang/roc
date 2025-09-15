# META
~~~ini
description=Add a variable with spaces
type=file
~~~
# SOURCE
~~~roc
module [add2]

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),LowerIdent(3:8-3:9),OpPlus(3:10-3:11),Int(3:17-3:18),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.18
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "add2"))))
	(statements
		(s-decl @3.1-3.18
			(p-ident @3.1-3.5 (raw "add2"))
			(e-binop @3.8-3.18 (op "+")
				(e-ident @3.8-3.9 (raw "x"))
				(e-int @3.17-3.18 (raw "2"))))))
~~~
# FORMATTED
~~~roc
module [add2]

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
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Error")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @3.8-3.18 (type "Error"))))
~~~
