# META
~~~ini
description=A simple module with no blanks
type=snippet
~~~
# SOURCE
~~~roc
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# EXPECTED
MODULE NOT FOUND - simple_module_no_blanks.md:1:1:1:17
UNDEFINED VARIABLE - simple_module_no_blanks.md:2:10:2:22
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**simple_module_no_blanks.md:1:1:1:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**simple_module_no_blanks.md:2:10:2:22:**
```roc
hello! = Stdout.line!("Hello")
```
         ^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "pf.Stdout"))
		(s-decl
			(p-ident (raw "hello!"))
			(e-apply
				(e-ident (raw "Stdout.line!"))
				(e-string
					(e-string-part (raw "Hello")))))
		(s-decl
			(p-ident (raw "world"))
			(e-string
				(e-string-part (raw "World"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "hello!"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-string
				(e-literal (string "Hello")))))
	(d-let
		(p-assign (ident "world"))
		(e-string
			(e-literal (string "World"))))
	(s-import (module "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Str")))
	(expressions
		(expr (type "Error"))
		(expr (type "Str"))))
~~~
