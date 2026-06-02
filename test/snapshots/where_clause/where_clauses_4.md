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
NOT IMPLEMENTED - where_clauses_4.md:1:1:1:1
UNSUPPORTED WHERE CLAUSE - where_clauses_4.md:4:9:4:17
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: ellipsis expression

**where_clauses_4.md:1:1:1:1:**
```roc
import Decode exposing [Decode]
```
^

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


**UNSUPPORTED WHERE CLAUSE**
The where clause syntax _Decode_ is not supported:
**where_clauses_4.md:4:9:4:17:**
```roc
	where [a.Decode]
```
	       ^^^^^^^^

This syntax was used for abilities, which have been removed from Roc. Use method constraints like `where [a.methodName(args) -> ret]` instead.

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
		(e-runtime-error (tag "not_implemented"))
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
