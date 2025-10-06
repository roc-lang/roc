# META
~~~ini
description=An empty module with singleline exposes
type=file:ModuleNonemptySingle.roc
~~~
# SOURCE
~~~roc
ModuleNonemptySingle := {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:21),OpColonEqual(1:22-1:24),OpenCurly(1:25-1:26),CloseCurly(1:26-1:27),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.27
	(type-module @1.1-1.21)
	(statements
		(s-type-decl @1.1-1.27
			(header @1.1-1.21 (name "ModuleNonemptySingle")
				(args))
			(ty-record @1.25-1.27))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.27
		(ty-header @1.1-1.21 (name "ModuleNonemptySingle"))
		(ty-record @1.25-1.27)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.27 (type "ModuleNonemptySingle")
			(ty-header @1.1-1.21 (name "ModuleNonemptySingle"))))
	(expressions))
~~~
