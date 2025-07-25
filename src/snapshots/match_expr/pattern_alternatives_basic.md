# META
~~~ini
description=Basic pattern alternatives with multiple tag patterns
type=file
~~~
# SOURCE
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
    Red | Green | Blue => "primary"
    Yellow | Orange | Purple => "secondary"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
UpperIdent(3:1-3:6),OpColon(3:7-3:8),OpenSquare(3:9-3:10),UpperIdent(3:10-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),Comma(3:26-3:27),UpperIdent(3:28-3:34),Comma(3:34-3:35),UpperIdent(3:36-3:42),Comma(3:42-3:43),UpperIdent(3:44-3:50),CloseSquare(3:50-3:51),
LowerIdent(5:1-5:5),OpColon(5:6-5:7),UpperIdent(5:8-5:13),OpArrow(5:14-5:16),UpperIdent(5:17-5:20),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),OpBar(6:8-6:9),LowerIdent(6:9-6:14),OpBar(6:14-6:15),KwMatch(6:16-6:21),LowerIdent(6:22-6:27),OpenCurly(6:28-6:29),
UpperIdent(7:5-7:8),OpBar(7:9-7:10),UpperIdent(7:11-7:16),OpBar(7:17-7:18),UpperIdent(7:19-7:23),OpFatArrow(7:24-7:26),StringStart(7:27-7:28),StringPart(7:28-7:35),StringEnd(7:35-7:36),
UpperIdent(8:5-8:11),OpBar(8:12-8:13),UpperIdent(8:14-8:20),OpBar(8:21-8:22),UpperIdent(8:23-8:29),OpFatArrow(8:30-8:32),StringStart(8:33-8:34),StringPart(8:34-8:43),StringEnd(8:43-8:44),
CloseCurly(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(file @1.1-9.2
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "kind"))))
	(statements
		(s-type-decl @3.1-3.51
			(header @3.1-3.6 (name "Color")
				(args))
			(ty-tag-union @3.9-3.51
				(tags
					(ty @3.10-3.13 (name "Red"))
					(ty @3.15-3.20 (name "Green"))
					(ty @3.22-3.26 (name "Blue"))
					(ty @3.28-3.34 (name "Yellow"))
					(ty @3.36-3.42 (name "Orange"))
					(ty @3.44-3.50 (name "Purple")))))
		(s-type-anno @5.1-5.20 (name "kind")
			(ty-fn @5.8-5.20
				(ty @5.8-5.13 (name "Color"))
				(ty @5.17-5.20 (name "Str"))))
		(s-decl @6.1-9.2
			(p-ident @6.1-6.5 (raw "kind"))
			(e-lambda @6.8-9.2
				(args
					(p-ident @6.9-6.14 (raw "color")))
				(e-match
					(e-ident @6.22-6.27 (raw "color"))
					(branches
						(branch @7.5-7.36
							(p-alternatives
								(p-tag @7.5-7.8 (raw "Red"))
								(p-tag @7.11-7.16 (raw "Green"))
								(p-tag @7.19-7.23 (raw "Blue")))
							(e-string @7.27-7.36
								(e-string-part @7.28-7.35 (raw "primary"))))
						(branch @8.5-8.44
							(p-alternatives
								(p-tag @8.5-8.11 (raw "Yellow"))
								(p-tag @8.14-8.20 (raw "Orange"))
								(p-tag @8.23-8.29 (raw "Purple")))
							(e-string @8.33-8.44
								(e-string-part @8.34-8.43 (raw "secondary"))))))))))
~~~
# FORMATTED
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
	Red | Green | Blue => "primary"
	Yellow | Orange | Purple => "secondary"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.5 (ident "kind"))
		(e-closure @6.8-9.2
			(e-lambda @6.8-9.2
				(args
					(p-assign @6.9-6.14 (ident "color")))
				(e-match @6.16-9.2
					(match @6.16-9.2
						(cond
							(e-lookup-local @6.22-6.27
								(p-assign @6.9-6.14 (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @7.5-7.8))
									(pattern (degenerate false)
										(p-applied-tag @7.11-7.16))
									(pattern (degenerate false)
										(p-applied-tag @7.19-7.23)))
								(value
									(e-string @7.27-7.36
										(e-literal @7.28-7.35 (string "primary")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @8.5-8.11))
									(pattern (degenerate false)
										(p-applied-tag @8.14-8.20))
									(pattern (degenerate false)
										(p-applied-tag @8.23-8.29)))
								(value
									(e-string @8.33-8.44
										(e-literal @8.34-8.43 (string "secondary"))))))))))
		(annotation @6.1-6.5
			(declared-type
				(ty-fn @5.8-5.20 (effectful false)
					(ty @5.8-5.13 (name "Color"))
					(ty @5.17-5.20 (name "Str"))))))
	(s-alias-decl @3.1-3.51
		(ty-header @3.1-3.6 (name "Color"))
		(ty-tag-union @3.9-3.51
			(ty @3.10-3.13 (name "Red"))
			(ty @3.15-3.20 (name "Green"))
			(ty @3.22-3.26 (name "Blue"))
			(ty @3.28-3.34 (name "Yellow"))
			(ty @3.36-3.42 (name "Orange"))
			(ty @3.44-3.50 (name "Purple")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.5 (type "Color -> Str")))
	(type_decls
		(alias @3.1-3.51 (type "Color")
			(ty-header @3.1-3.6 (name "Color"))))
	(expressions
		(expr @6.8-9.2 (type "Color -> Str"))))
~~~
