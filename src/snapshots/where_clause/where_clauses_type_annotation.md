# META
~~~ini
description=Simple type annotation with where clause
type=file
~~~
# SOURCE
~~~roc
module [convert]

convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),CloseSquare(1:16-1:17),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),OpArrow(3:13-3:15),LowerIdent(3:16-3:17),KwWhere(3:18-3:23),KwModule(3:24-3:30),NoSpaceOpenRound(3:30-3:31),LowerIdent(3:31-3:32),CloseRound(3:32-3:33),NoSpaceDotLowerIdent(3:33-3:38),OpColon(3:39-3:40),LowerIdent(3:41-3:42),OpArrow(3:43-3:45),LowerIdent(3:46-3:47),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:13),OpBar(4:13-4:14),LowerIdent(4:15-4:16),NoSpaceDotLowerIdent(4:16-4:21),NoSpaceOpenRound(4:21-4:22),CloseRound(4:22-4:23),EndOfFile(4:23-4:23),
~~~
# PARSE
~~~clojure
(file @1.1-4.23
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-lower-ident @1.9-1.16 (text "convert"))))
	(statements
		(s-type-anno @3.1-3.47 (name "convert")
			(ty-fn @3.11-3.17
				(ty-var @1.1-1.1 (raw "a"))
				(ty-var @1.1-1.1 (raw "b")))
			(where
				(method @3.24-3.47 (module-of "a") (name "to_b")
					(args
						(ty-var @1.1-1.1 (raw "a")))
					(ty-var @1.1-1.1 (raw "b")))))
		(s-decl @4.1-4.23
			(p-ident @4.1-4.8 (raw "convert"))
			(e-lambda @4.11-4.23
				(args
					(p-ident @4.12-4.13 (raw "a")))
				(e-field-access @4.15-4.23
					(e-ident @4.15-4.16 (raw "a"))
					(e-apply @4.16-4.23
						(e-ident @4.16-4.21 (raw "to_b"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "convert"))
		(e-lambda @4.11-4.23
			(args
				(p-assign @4.12-4.13 (ident "a")))
			(e-dot-access @4.15-4.23 (field "to_b")
				(receiver
					(e-lookup-local @4.15-4.16
						(p-assign @4.12-4.13 (ident "a"))))
				(args)))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.17 (effectful false)
					(ty-var @1.1-1.1 (name "a"))
					(ty-var @1.1-1.1 (name "b"))))))
	(s-type-anno @3.1-3.47 (name "convert")
		(ty-fn @3.11-3.17 (effectful false)
			(ty-var @1.1-1.1 (name "a"))
			(ty-var @1.1-1.1 (name "b")))
		(where
			(method @3.24-3.47 (module-of "a") (ident "to_b")
				(args
					(ty-var @1.1-1.1 (name "a")))
				(ty-var @1.1-1.1 (name "b")))))
	(ext-decl @3.24-3.47 (ident "module(a).to_b") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "{ to_b: b } -> b")))
	(expressions
		(expr @4.11-4.23 (type "{ to_b: b } -> b"))))
~~~
