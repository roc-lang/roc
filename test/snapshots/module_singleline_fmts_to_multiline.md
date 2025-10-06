# META
~~~ini
description=An empty module with a singleline exposes with trailing comma
type=file:ModuleSinglelineFmtsToMultiline.roc
~~~
# SOURCE
~~~roc
ModuleSinglelineFmtsToMultiline := {}

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:32),OpColonEqual(1:33-1:35),OpenCurly(1:36-1:37),CloseCurly(1:37-1:38),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.38
	(type-module @1.1-1.32)
	(statements
		(s-type-decl @1.1-1.38
			(header @1.1-1.32 (name "ModuleSinglelineFmtsToMultiline")
				(args))
			(ty-record @1.36-1.38))))
~~~
# FORMATTED
~~~roc
ModuleSinglelineFmtsToMultiline := {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.38
		(ty-header @1.1-1.32 (name "ModuleSinglelineFmtsToMultiline"))
		(ty-record @1.36-1.38)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.38 (type "ModuleSinglelineFmtsToMultiline")
			(ty-header @1.1-1.32 (name "ModuleSinglelineFmtsToMultiline"))))
	(expressions))
~~~
