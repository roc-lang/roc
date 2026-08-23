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
UNDECLARED TYPE - where_clauses_4.md:4:10:4:17
# PROBLEMS
── ✗ undeclared type ─────────────────────────────────── where_clauses_4.md:4:10

The type Decode is not declared in this scope.

where [a.Decode]
        ^^^^^^^

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
	(type-mod)
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
				(alias (mod-of "a")
					(ty (name "Decode")))))
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U8") (builtin))))
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a"))))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-malformed)))))
	(s-import (mod "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(List(U8)) -> List(Error)")))
	(expressions
		(expr (type "List(List(U8)) -> List(Error)"))))
~~~
