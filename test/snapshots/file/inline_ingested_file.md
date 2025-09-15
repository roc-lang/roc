# META
~~~ini
description=inline_ingested_file
type=file
~~~
# SOURCE
~~~roc
module [foo]

import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# EXPECTED
PARSE ERROR - inline_ingested_file.md:3:8:3:9
PARSE ERROR - inline_ingested_file.md:3:9:3:19
PARSE ERROR - inline_ingested_file.md:3:19:3:20
PARSE ERROR - inline_ingested_file.md:3:21:3:23
MODULE NOT FOUND - inline_ingested_file.md:4:1:4:12
UNDEFINED VARIABLE - inline_ingested_file.md:6:18:6:22
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:3:8:3:9:**
```roc
import "users.json" as data : Str
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:3:9:3:19:**
```roc
import "users.json" as data : Str
```
        ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:3:19:3:20:**
```roc
import "users.json" as data : Str
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:3:21:3:23:**
```roc
import "users.json" as data : Str
```
                    ^^


**MODULE NOT FOUND**
The module `Json` was not found in this Roc project.

You're attempting to use this module here:
**inline_ingested_file.md:4:1:4:12:**
```roc
import Json
```
^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**inline_ingested_file.md:6:18:6:22:**
```roc
foo = Json.parse(data)
```
                 ^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
KwImport(3:1-3:7),StringStart(3:8-3:9),StringPart(3:9-3:19),StringEnd(3:19-3:20),KwAs(3:21-3:23),LowerIdent(3:24-3:28),OpColon(3:29-3:30),UpperIdent(3:31-3:34),
KwImport(4:1-4:7),UpperIdent(4:8-4:12),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:11),NoSpaceDotLowerIdent(6:11-6:17),NoSpaceOpenRound(6:17-6:18),LowerIdent(6:18-6:22),CloseRound(6:22-6:23),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.23
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-malformed @3.1-3.9 (tag "incomplete_import"))
		(s-malformed @3.9-3.19 (tag "statement_unexpected_token"))
		(s-malformed @3.19-3.20 (tag "statement_unexpected_token"))
		(s-malformed @3.21-3.23 (tag "statement_unexpected_token"))
		(s-type-anno @3.24-3.34 (name "data")
			(ty @3.31-3.34 (name "Str")))
		(s-import @4.1-4.12 (raw "Json"))
		(s-decl @6.1-6.23
			(p-ident @6.1-6.4 (raw "foo"))
			(e-apply @6.7-6.23
				(e-ident @6.7-6.17 (raw "Json.parse"))
				(e-ident @6.18-6.22 (raw "data"))))))
~~~
# FORMATTED
~~~roc
module [foo]

data : Str
import Json

foo = Json.parse(data)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "foo"))
		(e-call @6.7-6.23
			(e-runtime-error (tag "ident_not_in_scope"))))
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
			(tag_name @1.1-1.1 (name "Err"))))
	(s-import @4.1-4.12 (module "Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "_a")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @6.7-6.23 (type "_a"))))
~~~
