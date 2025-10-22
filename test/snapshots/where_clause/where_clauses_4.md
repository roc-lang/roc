# META
~~~ini
description=where_clauses (4)
type=snippet
~~~
# SOURCE
~~~roc
import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where [a.Decode]
decodeThings = ...
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_4.md:1:1:1:32
# PROBLEMS
**MODULE NOT FOUND**
The module `Decode` was not found in this Roc project.

You're attempting to use this module here:
**where_clauses_4.md:1:1:1:32:**
```roc
import Decode exposing [Decode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,TripleDot,
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
		(s-type-anno (name "decodeThings")
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
				(alias (module-of "a") (name "Decode"))))
		(s-decl
			(p-ident (raw "decodeThings"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "decodeThings"))
		(e-not-implemented)
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U8") (builtin))))
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a"))))
			(where
				(alias (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "Decode")))))
	(s-import (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(List(Num(Int(Unsigned8)))) -> List(a)")))
	(expressions
		(expr (type "List(List(Num(Int(Unsigned8)))) -> List(a)"))))
~~~
