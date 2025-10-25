# META
~~~ini
description=Simple plaform module
type=file
~~~
# SOURCE
~~~roc
platform ""
	requires {} { main : Str -> Str }
	exposes []
	packages {}
	provides { entrypoint: "roc__entrypoint" }

entrypoint : Str -> Str
entrypoint = main
~~~
# EXPECTED
UNDEFINED VARIABLE - platform_header_str_simple.md:8:14:8:18
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `main` in this scope.
Is there an `import` or `exposing` missing up-top?

**platform_header_str_simple.md:8:14:8:18:**
```roc
entrypoint = main
```
             ^^^^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(rigids)
		(ty-record
			(anno-record-field (name "main")
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))))
		(exposes)
		(packages)
		(provides
			(record-field (name "entrypoint")
				(e-string
					(e-string-part (raw "roc__entrypoint"))))))
	(statements
		(s-type-anno (name "entrypoint")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "entrypoint"))
			(e-ident (raw "main")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "entrypoint"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (external-module "Str"))
				(ty-lookup (name "Str") (external-module "Str"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
