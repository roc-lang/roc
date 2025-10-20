# META
~~~ini
description=where_clauses (10)
type=snippet
~~~
# SOURCE
~~~roc
import Decode exposing [Decode]

decode_things # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where # after where
				[a.Decode]
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_10.md:1:1:1:32
# PROBLEMS
**MODULE NOT FOUND**
The module `Decode` was not found in this Roc project.

You're attempting to use this module here:
**where_clauses_10.md:1:1:1:32:**
```roc
import Decode exposing [Decode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,
OpColon,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
KwWhere,
OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "Decode")
			(exposing
				(exposed-upper-ident (text "Decode"))))
		(s-type-anno (name "decode_things")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-apply
						(ty (name "List"))
						(ty (name "U8"))))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a"))))
			(where
				(alias (module-of "a") (name "Decode"))))))
~~~
# FORMATTED
~~~roc
import Decode exposing [Decode]

decode_things # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where
				[
				a.Decode
			]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false))))
	(s-type-anno (name "decode_things")
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin))))
			(ty-apply (name "List") (builtin)
				(ty-rigid-var (name "a"))))
		(where
			(alias (module-of "a") (ident "Decode"))))
	(ext-decl (ident "a.Decode") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
