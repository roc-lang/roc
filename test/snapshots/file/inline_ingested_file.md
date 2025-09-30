# META
~~~ini
description=inline_ingested_file
type=file
~~~
# SOURCE
~~~roc
import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# EXPECTED
PARSE ERROR - inline_ingested_file.md:1:8:1:9
PARSE ERROR - inline_ingested_file.md:1:9:1:19
PARSE ERROR - inline_ingested_file.md:1:19:1:20
PARSE ERROR - inline_ingested_file.md:1:21:1:23
MISSING MAIN! FUNCTION - inline_ingested_file.md:1:1:4:23
MODULE NOT FOUND - inline_ingested_file.md:2:1:2:12
UNDEFINED VARIABLE - inline_ingested_file.md:4:18:4:22
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:8:1:9:**
```roc
import "users.json" as data : Str
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:9:1:19:**
```roc
import "users.json" as data : Str
```
        ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:19:1:20:**
```roc
import "users.json" as data : Str
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:21:1:23:**
```roc
import "users.json" as data : Str
```
                    ^^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**inline_ingested_file.md:1:1:4:23:**
```roc
import "users.json" as data : Str
import Json

foo = Json.parse(data)
```


**MODULE NOT FOUND**
The module `Json` was not found in this Roc project.

You're attempting to use this module here:
**inline_ingested_file.md:2:1:2:12:**
```roc
import Json
```
^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**inline_ingested_file.md:4:18:4:22:**
```roc
foo = Json.parse(data)
```
                 ^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),StringStart(1:8-1:9),StringPart(1:9-1:19),StringEnd(1:19-1:20),KwAs(1:21-1:23),LowerIdent(1:24-1:28),OpColon(1:29-1:30),UpperIdent(1:31-1:34),
KwImport(2:1-2:7),UpperIdent(2:8-2:12),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),UpperIdent(4:7-4:11),NoSpaceDotLowerIdent(4:11-4:17),NoSpaceOpenRound(4:17-4:18),LowerIdent(4:18-4:22),CloseRound(4:22-4:23),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.23
	(type-module @1.1-1.7)
	(statements
		(s-malformed @1.1-1.9 (tag "incomplete_import"))
		(s-malformed @1.9-1.19 (tag "statement_unexpected_token"))
		(s-malformed @1.19-1.20 (tag "statement_unexpected_token"))
		(s-malformed @1.21-1.23 (tag "statement_unexpected_token"))
		(s-type-anno @1.24-1.34 (name "data")
			(ty @1.31-1.34 (name "Str")))
		(s-import @2.1-2.12 (raw "Json"))
		(s-decl @4.1-4.23
			(p-ident @4.1-4.4 (raw "foo"))
			(e-apply @4.7-4.23
				(e-ident @4.7-4.17 (raw "Json.parse"))
				(e-ident @4.18-4.22 (raw "data"))))))
~~~
# FORMATTED
~~~roc
data : Str
import Json

foo = Json.parse(data)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-call @4.7-4.23
			(e-lookup-external @4.7-4.17
				(module-idx "0")
				(target-node-idx "0"))
			(e-runtime-error (tag "ident_not_in_scope"))))
	(s-import @2.1-2.12 (module "Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "_a")))
	(expressions
		(expr @4.7-4.23 (type "_a"))))
~~~
