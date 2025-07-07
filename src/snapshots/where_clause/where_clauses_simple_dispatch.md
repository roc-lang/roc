# META
~~~ini
description=Simple where clause with single constraint
type=file
~~~
# SOURCE
~~~roc
module [stringify]

stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:18),CloseSquare(1:18-1:19),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:10),OpColon(3:11-3:12),LowerIdent(3:13-3:14),OpArrow(3:15-3:17),UpperIdent(3:18-3:21),KwWhere(3:22-3:27),KwModule(3:28-3:34),NoSpaceOpenRound(3:34-3:35),LowerIdent(3:35-3:36),CloseRound(3:36-3:37),NoSpaceDotLowerIdent(3:37-3:44),OpColon(3:45-3:46),LowerIdent(3:47-3:48),OpArrow(3:49-3:51),UpperIdent(3:52-3:55),Newline(1:1-1:1),
LowerIdent(4:1-4:10),OpAssign(4:11-4:12),OpBar(4:13-4:14),LowerIdent(4:14-4:19),OpBar(4:19-4:20),LowerIdent(4:21-4:26),NoSpaceDotLowerIdent(4:26-4:33),NoSpaceOpenRound(4:33-4:34),CloseRound(4:34-4:35),EndOfFile(4:35-4:35),
~~~
# PARSE
~~~clojure
(file @1.1-4.35
	(module @1.1-1.19
		(exposes @1.8-1.19
			(exposed-lower-ident (text "stringify"))))
	(statements
		(s-type-anno @1.1-1.1 (name "stringify")
			(ty-fn @3.13-3.21
				(ty-var @3.13-3.14 (raw "a"))
				(ty @3.18-3.21 (name "Str")))
			(where
				(method @1.1-1.1 (module-of "a") (name "to_str")
					(args
						(ty-var @3.47-3.48 (raw "a")))
					(ty @3.52-3.55 (name "Str")))))
		(s-decl @4.1-4.35
			(p-ident @4.1-4.10 (raw "stringify"))
			(e-lambda @4.13-4.35
				(args
					(p-ident @4.14-4.19 (raw "value")))
				(e-field-access @4.21-4.35
					(e-ident @4.21-4.26 (raw "value"))
					(e-apply @4.26-4.35
						(e-ident @4.26-4.33 (raw "to_str"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.10 (ident "stringify"))
		(e-lambda @4.13-4.35
			(args
				(p-assign @4.14-4.19 (ident "value")))
			(e-dot-access @4.21-4.35 (field "to_str")
				(receiver
					(e-lookup-local @4.21-4.26
						(pattern @4.14-4.19)))
				(args)))
		(annotation @4.1-4.10
			(declared-type
				(ty-fn @3.13-3.21 (effectful false)
					(ty-var @3.13-3.14 (name "a"))
					(ty @3.18-3.21 (name "Str")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.10 (type "{ to_str: Str } -> Str")))
	(expressions
		(expr @4.13-4.35 (type "{ to_str: Str } -> Str"))))
~~~
