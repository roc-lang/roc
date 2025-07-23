# META
~~~ini
description=Nominal type with where clause constraint
type=file
~~~
# SOURCE
~~~roc
module [Cache]

Cache(k, v) := Dict(U64, v)
	where
		module(k).hash : k -> U64
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),CloseSquare(1:14-1:15),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),Comma(3:8-3:9),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),OpColonEqual(3:13-3:15),UpperIdent(3:16-3:20),NoSpaceOpenRound(3:20-3:21),UpperIdent(3:21-3:24),Comma(3:24-3:25),LowerIdent(3:26-3:27),CloseRound(3:27-3:28),
KwWhere(4:2-4:7),
KwModule(5:3-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),CloseRound(5:11-5:12),NoSpaceDotLowerIdent(5:12-5:17),OpColon(5:18-5:19),LowerIdent(5:20-5:21),OpArrow(5:22-5:24),UpperIdent(5:25-5:28),EndOfFile(5:28-5:28),
~~~
# PARSE
~~~clojure
(file @1.1-5.28
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-upper-ident @1.9-1.14 (text "Cache"))))
	(statements
		(s-type-decl @3.1-5.28
			(header @3.1-3.12 (name "Cache")
				(args
					(ty-var @3.7-3.8 (raw "k"))
					(ty-var @3.10-3.11 (raw "v"))))
			(ty-apply @3.16-3.28
				(ty @3.16-3.20 (name "Dict"))
				(ty @3.21-3.24 (name "U64"))
				(ty-var @3.26-3.27 (raw "v")))
			(where
				(method @5.3-5.28 (module-of "k") (name "hash")
					(args
						(ty-var @5.20-5.21 (raw "k")))
					(ty @5.25-5.28 (name "U64")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @3.1-5.28
		(type-header (name "Cache")
			(args
				(ty-var @3.7-3.8 (name "k"))
				(ty-var @3.10-3.11 (name "v"))))
		(ty-apply @3.16-3.28 (symbol "Dict")
			(ty @3.21-3.24 (name "U64"))
			(ty-var @3.26-3.27 (name "v")))
		(where
			(where-clause
				(type "mod-method")
				(var-name "k")
				(method-name "hash"))))
	(external-decl (qualified-name "module(k).hash") (module-name "module(k)") (local-name "hash") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @3.1-5.28 (type "Error")
			(type-header (name "Cache")
				(args
					(ty-var @3.7-3.8 (name "k"))
					(ty-var @3.10-3.11 (name "v"))))))
	(expressions))
~~~
