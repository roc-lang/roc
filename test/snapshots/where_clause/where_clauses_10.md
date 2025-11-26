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
NIL
# PROBLEMS
NIL
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
				[a.Decode]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "decode_things"))
		(e-anno-only)
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
		(patt (type "List(List(U8)) -> List(a)")))
	(expressions
		(expr (type "List(List(U8)) -> List(a)"))))
~~~
