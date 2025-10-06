# META
~~~ini
description=An empty module with no exposes
type=file:ModuleEmpty.roc
~~~
# SOURCE
~~~roc
ModuleEmpty := {}

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:12),OpColonEqual(1:13-1:15),OpenCurly(1:16-1:17),CloseCurly(1:17-1:18),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.18
	(type-module @1.1-1.12)
	(statements
		(s-type-decl @1.1-1.18
			(header @1.1-1.12 (name "ModuleEmpty")
				(args))
			(ty-record @1.16-1.18))))
~~~
# FORMATTED
~~~roc
ModuleEmpty := {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.18
		(ty-header @1.1-1.12 (name "ModuleEmpty"))
		(ty-record @1.16-1.18)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.18 (type "ModuleEmpty")
			(ty-header @1.1-1.12 (name "ModuleEmpty"))))
	(expressions))
~~~
