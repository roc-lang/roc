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
UNSUPPORTED WHERE CLAUSE - where_clauses_10.md:7:6:7:14
DECLARATION HAS NO VALUE - where_clauses_10.md:3:1:7:15
# PROBLEMS

┌──────────────────────────┐
│ UNSUPPORTED WHERE CLAUSE ├─ The where clause syntax Decode is not ──────────┐
└┬─────────────────────────┘  supported.                                      │
 │                                                                            │
 │  [a.Decode]                                                                │
 │   ‾‾‾‾‾‾‾‾                                                                 │
 └─────────────────────────────────────────────────── where_clauses_10.md:7:6 ┘

    This syntax was used for abilities, which have been removed from Roc. Use
    method constraints like `where [a.methodName(args) -> ret]` instead.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  decode_things # After member name                                         │
 │      : # After colon                                                       │
 │          List(List(U8)) -> List(a) # After anno                            │
 │              where # after where                                           │
 │                  [a.Decode]                                                │
 │                                                                            │
 └─────────────────────────────────────────────────── where_clauses_10.md:3:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

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
			where [
				a.Decode,
			]
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
