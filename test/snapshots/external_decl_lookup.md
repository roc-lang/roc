# META
~~~ini
description=External declaration lookup from json mod
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external mod!")
    Stdout.line!(result)
}
~~~
# EXPECTED
DUPLICATE DEFINITION - external_decl_lookup.md:4:1:4:17
DOES NOT EXIST - external_decl_lookup.md:8:19:8:23
DOES NOT EXIST - external_decl_lookup.md:9:12:9:17
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Json` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import json.Json                                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └─────────────────────────────────────────────── external_decl_lookup.md:4:1 ┘

    In this scope, `Json` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  app [main!] { pf: platform "../basic-cli/platform.roc" }             │
      │  ‾                                                                    │
      └────────────────────────────────────────── external_decl_lookup.md:1:1 ┘


┌────────────────┐
│ DOES NOT EXIST ├─ `utf8` was not found in `Json`. ──────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  result = Json.utf8("Hello from external mod!")                            │
 │                ‾‾‾‾                                                        │
 └────────────────────────────────────────────── external_decl_lookup.md:8:19 ┘

    Check that `utf8` is spelled correctly and that `Json` exposes it.


┌────────────────┐
│ DOES NOT EXIST ├─ `line!` was not found in `Stdout`. ───────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  Stdout.line!(result)                                                      │
 │         ‾‾‾‾‾                                                              │
 └────────────────────────────────────────────── external_decl_lookup.md:9:12 ┘

    Check that `line!` is spelled correctly and that `Stdout` exposes it.

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
									(e-string-part (raw "Hello from external mod!")))))
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
	result = Json.utf8("Hello from external mod!")
	Stdout.line!(result)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-import (mod "pf.Stdout")
		(exposes))
	(s-import (mod "json.Json")
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
