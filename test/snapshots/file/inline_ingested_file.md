# META
~~~ini
description=inline_ingested_file
type=snippet
~~~
# SOURCE
~~~roc
import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# EXPECTED
FILE NOT FOUND - inline_ingested_file.md:1:1:1:34
DUPLICATE DEFINITION - inline_ingested_file.md:2:1:2:12
MISSING METHOD - inline_ingested_file.md:4:7:4:17
# PROBLEMS
── ✗ file not found ──────────────────────────────── inline_ingested_file.md:1:1

The file users.json was not found.

import "users.json" as data : Str
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Make sure the file exists relative to your source file:

── ● duplicate definition ────────────────────────── inline_ingested_file.md:2:1

The name Json is being redeclared here:

import Json
^^^^^^^^^^^

In this scope, Json was already defined in inline_ingested_file.md:1:1:

import "users.json" as data : Str
^

── ✗ missing method ──────────────────────────────── inline_ingested_file.md:4:7

This is trying to dispatch a method named parser_for on an unresolved type
variable, but unresolved type variables have no methods.

foo = Json.parse(data)
      ^^^^^^^^^^

Hint: You can replace this static dispatch call with an ordinary function call,
or force the type variable to become more concrete—for example, by adding a
type annotation that narrows its type to something that actually has methods.

# TOKENS
~~~zig
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,
KwImport,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-file-import
			(path "users.json")
			(name "data")
			(type "Str"))
		(s-import (raw "Json"))
		(s-decl
			(p-ident (raw "foo"))
			(e-apply
				(e-ident (raw "Json.parse"))
				(e-ident (raw "data"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "data"))
		(e-runtime-error (tag "file_import_not_found")))
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-import (mod "Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
