# META
~~~ini
description=External declaration lookup from json module
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external module!")
    Stdout.line!(result)
}
~~~
# EXPECTED
MODULE NOT FOUND - external_decl_lookup.md:3:1:3:17
MODULE NOT FOUND - external_decl_lookup.md:4:1:4:17
UNDEFINED VARIABLE - external_decl_lookup.md:8:14:8:23
UNDEFINED VARIABLE - external_decl_lookup.md:9:5:9:17
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**external_decl_lookup.md:3:1:3:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**external_decl_lookup.md:4:1:4:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `utf8` in this scope.
Is there an `import` or `exposing` missing up-top?

**external_decl_lookup.md:8:14:8:23:**
```roc
    result = Json.utf8("Hello from external module!")
```
             ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**external_decl_lookup.md:9:5:9:17:**
```roc
    Stdout.line!(result)
```
    ^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
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
		(s-import (raw "json.Json"))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "Json.utf8"))
								(e-string
									(e-string-part (raw "Hello from external module!")))))
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-ident (raw "result")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
	# This should create an external declaration for json.Json.utf8
	result = Json.utf8("Hello from external module!")
	Stdout.line!(result)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "result"))
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-string
							(e-literal (string "Hello from external module!")))))
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local
						(p-assign (ident "result")))))))
	(s-import (module "pf.Stdout")
		(exposes))
	(s-import (module "json.Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "_arg -> Error"))))
~~~
