# META
~~~ini
description=Type mismatch - pure annotation with effectful body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# This should be a type error: pure annotation but effectful body
bad_function : Str -> {}
bad_function = |msg| Stdout.line!(msg)

main! = bad_function("This should fail")
~~~
# EXPECTED
MODULE NOT FOUND - pure_annotation_effectful_body_error.md:3:1:3:17
UNDEFINED VARIABLE - pure_annotation_effectful_body_error.md:7:22:7:34
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**pure_annotation_effectful_body_error.md:3:1:3:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**pure_annotation_effectful_body_error.md:7:22:7:34:**
```roc
bad_function = |msg| Stdout.line!(msg)
```
                     ^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import (raw "pf.Stdout"))
		(s-type-anno (name "bad_function")
			(ty-fn
				(ty (name "Str"))
				(ty-record)))
		(s-decl
			(p-ident (raw "bad_function"))
			(e-lambda
				(args
					(p-ident (raw "msg")))
				(e-apply
					(e-ident (raw "Stdout.line!"))
					(e-ident (raw "msg")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-apply
				(e-ident (raw "bad_function"))
				(e-string
					(e-string-part (raw "This should fail")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "bad_function"))
		(e-lambda
			(args
				(p-assign (ident "msg")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "msg")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-call
			(e-lookup-local
				(p-assign (ident "bad_function")))
			(e-string
				(e-literal (string "This should fail")))))
	(s-import (module "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> {  }"))
		(patt (type "{  }")))
	(expressions
		(expr (type "Str -> {  }"))
		(expr (type "{  }"))))
~~~
